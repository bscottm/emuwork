{-| General-purpose system memory, for both traditional memory and for port-based I/O systems.

Memory is a collection of address regions ('addrType') stored in an 'IntervalMap'. The 'IntervalMap' stores the start
and end addresses for the region; each region may hold a `RAMRegion` (random access memory), a `ROMRegion` (read-only
memory) or a device (`DevRegion`). Both RAM and ROM regions store content as vectors of a `wordType`.
Devices manage content as they see fit, although they may only return or store a `wordType`-sized item.

Memory regions __/may not/__ overlap; they __/must/__ be distinct.

Writing to a RAM region places the modified word into a priority search queue that acts as a LRU cache.
This amortizes the penalty from updating the underlying content vector until the queue's size is
exceeded (currently 71 elements.)

How to use: Add regions to an `initialMemorySystem` via `mkRAMRegion`, `mkROMRegion` or `mkDevRegion`. Example snippet
from the testing code that creates two RAM regions, 0x0-0x0fff and 0x1400-0x23ff:

> type TestMemSystem = M.MemorySystem Word16 Word8
>
> let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 M.initialMemorySystem :: TestMemSystem)
> in  ...

Alternatively, you could use `mempty` instead of `initialMemorySystem`:

> let msys  = M.mkRAMRegion 0x1400 0x1000 (M.mkRAMRegion 0 0x1000 mempty :: TestMemSystem)
> in  ...

 -}

module Machine.MemorySystem
  ( -- * Exported types
    MemorySystem(..)
  , MemRead
  , MemReadN
  -- * 'Monoid' interface (see 'MemorySystem' instance for 'Monoid')
  , initialMemorySystem
  , combineMemorySystems
  -- * Memory Regions
  , mkRAMRegion
  , mkROMRegion
  , mkDevRegion
  -- * Memory read primitives
  , mRead
  , mReadN
  -- * Memory write primitives
  , mWrite
  , mWriteN
  , mPatch
  -- * Utility functions
  , countRegions
  , regionList
  , sanityCheck
  ) where

import           Prelude                          hiding (words)
import           Control.Arrow                    (first, (***))
import           Control.Lens                     (Lens', over, to, view, views, (%~), (&), (+~), (-~), (.~), (^.), (|>), _2)
import           Control.Monad.Trans.State.Strict (state, execState)
import           Control.Monad                    (sequence)
import           Data.Semigroup                   (Semigroup)
import qualified Data.Foldable                    as Fold (foldl')
import qualified Data.IntervalMap.Strict          as IM
import qualified Data.IntervalMap.Interval        as I
import           Data.Maybe                      (fromMaybe)
import           Data.Vector.Unboxed             (Vector, (!), (//))
import qualified Data.Vector.Unboxed             as DVU
import qualified Data.OrdPSQ                     as OrdPSQ

#if defined(TEST_DEBUG)
import Debug.Trace
import Text.Printf
#endif

import qualified Machine.Device                  as D
import           Machine.Utils

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Types
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Shorthand for the memory region interval map
type MemRegionMap addrType wordType = IM.IntervalMap addrType (MemoryRegion addrType wordType)

data MemorySystem addrType wordType where
  MSys ::
    { _regions    :: MemRegionMap addrType wordType
    -- ^ Region interval map
    } -> MemorySystem addrType wordType

instance ( Show addrType
         , Show wordType
         , DVU.Unbox wordType
         , Integral wordType
         )
         => Show (MemorySystem addrType wordType) where
  show msys = "MemorySystem(" ++ show (msys ^. regions) ++ ")"

-- Admit MemorySystem to the Semigroup class (prerequisite for Monoid)
instance (Integral addrType) =>
         Semigroup (MemorySystem addrType wordType) where
  (<>) = combineMemorySystems

-- Admit MemorySystem into the Monoid class
instance ( Integral addrType
         , Integral wordType
         )
         => Monoid (MemorySystem addrType wordType) where
  mempty  = initialMemorySystem
  mappend = combineMemorySystems

-- | Lens for the memory region map inside a 'MemorySystem'
regions :: Lens' (MemorySystem addrType wordType) (MemRegionMap addrType wordType)
regions f msys@MSys{ _regions = rgn } = (\regions' -> msys { _regions = regions' }) <$> f rgn

-- | Create an empty memory system (alternately: 'mempty')
initialMemorySystem :: MemorySystem addrType wordType
initialMemorySystem = MSys { _regions  = IM.empty }

-- | Combine two memory systems, if they don't overlap. If they overlap, an error is raised.
combineMemorySystems :: (Integral addrType)
                     => MemorySystem addrType wordType
                     -> MemorySystem addrType wordType
                     -> MemorySystem addrType wordType
combineMemorySystems msysA msysB
  | IM.null (IM.intersection (msysA ^. regions) (msysB ^. regions))
  = over regions (mappend (msysB ^. regions )) msysA
  | otherwise
  = error "MemorySystem:combineMemorySystems: Overlapping regions between memory systems"

-- | A memory region
data MemoryRegion addrType wordType where
  EmptyRegion :: MemoryRegion addrType wordType
  RAMRegion ::
    { _contents       :: ( DVU.Unbox wordType
                         , Integral wordType
                         )
                      => Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    , _writesPending  :: LRUWriteCache wordType
    -- ^ Pending write LRU cache
    , _nWritesPending :: {-# UNPACK #-} !Int
    -- ^ Size of the LRU cache (avoids the $O(n)$ penalty for calling 'OrdPSQ.size')
    } -> MemoryRegion addrType wordType
  ROMRegion ::
    { _contents       :: ( DVU.Unbox wordType
                         , Integral wordType
                         )
                      => Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  DevRegion ::
    { _rgnDevice      :: D.Device addrType wordType
    } -> MemoryRegion addrType wordType

instance ( Show addrType
         , Show wordType
         , Integral wordType
         , DVU.Unbox wordType
         )
         => Show (MemoryRegion addrType wordType) where
  show EmptyRegion                  = "EmptyRegion"
  show (RAMRegion ctnt wpend npend) = "RAMRegion(" ++ show wpend ++ ", " ++ show ctnt ++ " nPend " ++ show npend ++ ")"
  show (ROMRegion ctnt)             = "ROMRegion(" ++ show ctnt ++ ")"
  show (DevRegion _dev)             = "DevRegion()"

-- | Lens for a memory region's contents
contents :: ( DVU.Unbox wordType
            , Integral wordType
            )
         => Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion@(RAMRegion ctnt _ _) = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@(ROMRegion ctnt)     = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion                      = mregion <$ f DVU.empty

-- | Lens for the pending write LRU queue.
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache wordType)
writesPending f mregion@RAMRegion{}    = (\wp -> mregion { _writesPending = wp }) <$> f (_writesPending mregion)
-- For types other than 'RAMRegion', it's just the original memory region.
writesPending f mregion@EmptyRegion{}  = mregion <$ f (initialLRU 0)
writesPending f mregion@ROMRegion{}    = mregion <$ f (initialLRU 0)
writesPending f mregion@DevRegion{}    = mregion <$ f (initialLRU 0)

-- | Lens for number of elements in the LRU queue
nWritesPending :: Lens' (MemoryRegion addrType wordType) Int
nWritesPending f mregion@RAMRegion{} = (\n -> mregion { _nWritesPending = n }) <$> f (_nWritesPending mregion)
nWritesPending f mregion             = mregion <$ f 0


-- | Return type for 'mRead'. It has to be this way because devices __can__ (and usually __do__) change
-- state when read, producing an altered 'MemorySystem'
type MemRead addrType wordType = (wordType, MemorySystem addrType wordType)
-- | Return type for 'mReadN'. Devices __can__ and usually __do__ change state when read, producing an altered
-- 'MemorySystem'.
type MemReadN addrType wordType = (Vector wordType, MemorySystem addrType wordType)


-- | LRU write cache's "tick" (time/usage tracking) type
type Tick = Int
-- | Write cache's maximum occupancy type
type Size = Int
-- | Least Recently Used (LRU) write cache. Pending writes are queued in the LRU cache to avoid having to recreate or
-- modify a RAM region's vector, which can be expensive when new values are written to RAM.
data LRUWriteCache wordType where
  LRUWriteCache ::
    { _lrucNextTick :: {-# UNPACK #-} !Tick
    , _lrucPsq      ::                !(OrdPSQ.OrdPSQ Int Tick wordType)
    , _lrucMaxSize  :: {-# UNPACK #-} !Size
    } -> LRUWriteCache wordType
    deriving (Show)

lrucPsq :: Lens' (LRUWriteCache wordType) (OrdPSQ.OrdPSQ Int Tick wordType)
lrucPsq f psq = (\psq' -> psq { _lrucPsq = psq' }) <$> f (_lrucPsq psq)

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- MemorySystem code
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Create a new RAM memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkRAMRegion :: ( Integral addrType
               , ShowHex addrType
               )
            => addrType
            -- ^ RAM region start address
            -> Int
            -- ^ Length of the RAM region
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkRAMRegion sa len =
  insertMemRegion sa (sa + fromIntegral len)
                  RAMRegion { _contents       = DVU.replicate len 0
                            , _writesPending  = initialLRU maxPending
                            , _nWritesPending = 0
                            }

-- | Create a new read-only (ROM) memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkROMRegion :: ( Integral addrType
               , ShowHex addrType
               , DVU.Unbox wordType
               )
            => addrType
            -- ^ Region's start address
            -> Vector wordType
            -- ^ ROM image to insert as the region's contents
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkROMRegion sa romImg =
  insertMemRegion sa (sa + fromIntegral (DVU.length romImg)) ROMRegion { _contents = romImg }

-- | Create a new device memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkDevRegion :: ( Integral addrType
               , ShowHex addrType
               , DVU.Unbox wordType
               )
            => addrType
            -- ^ Region's starting address
            -> Int
            -- ^ Region length
            -> D.Device addrType wordType
            -- ^ The device itself
            -> MemorySystem addrType wordType
            -- ^ The original memory system
            -> MemorySystem addrType wordType
            -- ^ The result memory system
mkDevRegion sa len dev = insertMemRegion sa (sa + fromIntegral len) (DevRegion dev)

-- | Internal common code that inserts memory regions.
insertMemRegion :: ( Integral addrType
                   , ShowHex addrType
                   )
                => addrType
                -> addrType
                -> MemoryRegion addrType wordType
                -> MemorySystem addrType wordType
                -> MemorySystem addrType wordType
insertMemRegion sa ea newRegion msys
  | IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
  = over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys
  | otherwise
  = error ("mkRAMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")


-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned.
mRead :: ( Integral addrType
         , Integral wordType
         , DVU.Unbox wordType
         )
      => addrType
      -- ^ Address to read
      -> MemorySystem addrType wordType
      -- ^ Current memory system state
      -> MemRead addrType wordType
      -- ^ Value read and updated system state
mRead addr msys =
  let mregions                 = msys ^. regions
      ensureValue []           = 0
      ensureValue (v:_)        = v
      getContent memvals iv mr =
        let addrOffset = addr - I.lowerBound iv
        in  case mr of
              -- Arrows provide a neat shorthand to create the return tuple: '***' applies the RH function (val |>)
              -- to append the value read from the device to the accumulated list of values, and the LH data
              -- constructor to re-wrap the new device state.
              DevRegion{ _rgnDevice = (D.Device dev' reset reader writer) } ->
                (memvals |>) *** (\dev -> DevRegion (D.Device dev reset reader writer)) $ reader addrOffset dev'
              _ ->
                let val = fromMaybe (view contents mr ! fromIntegral addrOffset)
                                    (views writesPending (lookupPendingWrite $ fromIntegral addrOffset) mr)
                in  (memvals |> val, mr)
  in  ensureValue *** (\updMRs -> msys & regions %~ updDevRegions updMRs) $
          IM.mapAccumWithKey getContent [] (IM.splitAt mregions addr ^. _2)


-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: ( Integral addrType
          , Integral wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , PrintfArg wordType
          , Show addrType
          , Show wordType
#endif
          , DVU.Unbox wordType
          )
       => addrType
       -- ^ Starting address
       -> Int
       -- ^ Number of words to read
       -> MemorySystem addrType wordType
       -- ^ The memory system from which to read
       -> MemReadN addrType wordType
       -- ^ Contents read from memory and updated system state
mReadN sa nWords msys
  {- Trivial optimizations: -}
  | nWords == 1
  = first DVU.singleton (mRead sa msys)
  {- Otherwise, have to do some work. -}
  | otherwise
  = (finalVec DVU.++ trailingPad, msys & regions %~ updDevRegions updMem)
  where
    (result, updMem)            = IM.mapAccumWithKey collectReads initAccum memRegions
    finalVec                    = readVec result DVU.empty
    trailingLen                 = nWords - DVU.length finalVec
    trailingPad                 = if trailingLen <= 0
                                  then DVU.empty
                                  else DVU.replicate trailingLen 0
    memRegions                  = IM.intersecting (msys ^. regions) (I.ClosedInterval sa (sa + fromIntegral (nWords - 1)))
    initAccum                   = ReadAccum { lastAddr = sa, wordsRem = nWords, readVec = (DVU.empty DVU.++) }


-- | Update a memory region interval map with only the device regions in the updating map.
updDevRegions :: ( Ord addrType
                 )
              => MemRegionMap addrType wordType
              -> MemRegionMap addrType wordType
              -> MemRegionMap addrType wordType
updDevRegions updRegions {-origRegions-} =
  execState $ sequence [state (\m -> ((), onlyDevRegions v k m)) | (k, v) <- IM.assocs updRegions]
  where
    onlyDevRegions dev@DevRegion{}  k mreg = IM.update (const (Just dev)) k mreg
    onlyDevRegions _mr             _k mreg = mreg


-- | Accumulator for 'mReadN' within 'IM.mapAccumWithKey'. Keeps track of the last address processed to detect when to zero pad
-- the resulting vector. A sectioned function accumulates the resulting vector, analogous to 'ShowS', thereby reducing the
-- append ('DVU.++') overhead to linear time.
data ReadAccum addrType wordType where
  ReadAccum ::
    { lastAddr :: addrType
    , wordsRem :: Int
    , readVec  :: Vector wordType -> Vector wordType
    } -> ReadAccum addrType wordType


-- | Helper function that collects memory reads over multiple intervals, filling in gaps between intervals with zeroes.
--
-- NOTE: This is a top-level function just to make debugging type checking easier. It's a bit long to be an internal
-- function to mReadN.
collectReads :: ( Integral addrType
                , Integral wordType
                , DVU.Unbox wordType
#if defined(TEST_DEBUG)
                , PrintfArg addrType
                , PrintfArg wordType
                , Show addrType
                , Show wordType
#endif
                )
             => ReadAccum addrType wordType
             -> I.Interval addrType
             -> MemoryRegion addrType wordType
             -> (ReadAccum addrType wordType, MemoryRegion addrType wordType)
collectReads accum iv mr
#if defined(TEST_DEBUG)
  | trace showProgress False
  = undefined
#endif
  | saddr < sRegion
  = collectReads fillGap iv mr
  | wordsRem accum <= 0
  = error ("mReadN: Words remaining leq 0: " ++ show (wordsRem accum))
  | otherwise
  = case mr of
      DevRegion{ _rgnDevice = devRegion } ->
        let (devReads, devState) = D.devReadSeq (saddr - sRegion) mLen devRegion
        in  (ReadAccum { lastAddr = saddr + fromIntegral mLen
                       , wordsRem = wordsRem accum - fromIntegral mLen
                       , readVec = (readVec accum devReads DVU.++)
                       }
            , DevRegion devState
        )
      _                  ->
        (ReadAccum { lastAddr = saddr + fromIntegral mLen
                   , wordsRem = wordsRem accum  - fromIntegral mLen
                   , readVec = (readVec accum mcontents DVU.++)
                   }
        , mr
        )
  where
    saddr      = lastAddr accum
    -- eaddr      = saddr + fromIntegral mLen - 1
    sRegion    = I.lowerBound iv
    sOffs      = fromIntegral (saddr - sRegion)
    eRegion    = I.upperBound iv
    mLen       = min (wordsRem accum) (fromIntegral (eRegion - saddr))
    padLen     = sRegion - saddr
    zeroPad    = DVU.replicate (fromIntegral padLen) 0
    mcontents  = views contents (DVU.slice sOffs (fromIntegral mLen)) mr // pendWrites
    pendWrites = [(addrOffs - sOffs, word) | (addrOffs, word) <- views writesPending getPendingWrites mr]
      where
        getPendingWrites = views lrucPsq filterAddrs
        filterAddrs psq' = [(addrOffs, val) | (addrOffs, _, val) <- OrdPSQ.toList psq'
                                            , addrOffs - sOffs < fromIntegral mLen]
    fillGap    = ReadAccum { lastAddr = sRegion
                           , wordsRem = wordsRem accum - fromIntegral padLen
                           , readVec = (readVec accum zeroPad DVU.++)
                           }
#if defined(TEST_DEBUG)
    showProgress = printf "lastAddr 0x%04x wordsRem %5d sRegion 0x%04x eRegion 0x%04x padLen %5d mLen %5d sOffs %5d"
                    (lastAddr accum)
                    (wordsRem accum)
                    sRegion
                    eRegion
                    padLen
                    mLen
                    sOffs
#endif


-- | Write a word into the memory system: inserts the word into the pending write LRU cache, flushing the cache to make
-- space if necessary, commiting updates to the underlying region's contents. If the memory region is not 'readWrite',
-- then nothing happens.
mWrite :: ( Integral addrType
          , Integral wordType
          , Show wordType
          , DVU.Unbox wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , Show addrType
          , PrintfArg wordType
          , Show wordType
#endif
          )
       => addrType
       -- ^ Address to write to
       -> wordType
       -- ^ Value to write
       -> MemorySystem addrType wordType
       -- ^ Current system state
       -> MemorySystem addrType wordType
       -- ^ Updated system state
mWrite addr val = doWrite True addr (DVU.singleton val)


-- | Write a block of memory, respecting the memory system's type
mWriteN :: ( Integral addrType
           , Integral wordType
           , Show wordType
           , DVU.Unbox wordType
#if defined(TEST_DEBUG)
           , PrintfArg addrType
           , Show addrType
           , PrintfArg wordType
           , Show wordType
#endif
           )
        => addrType
        -- ^ Starting address
        -> Vector wordType
        -- ^ Content to write
        -> MemorySystem addrType wordType
        -- ^ Current system state
        -> MemorySystem addrType wordType
        -- ^ Updated system state
mWriteN = doWrite True


-- | Patch (forcibly overwrite) memory. This does not obey or check the 'readWrite' flag and will truncate the memory patch
-- if it extends beyond a region's upper bound.
mPatch :: ( Integral addrType
          , Integral wordType
          , Show wordType
          , DVU.Unbox wordType
#if defined(TEST_DEBUG)
          , PrintfArg addrType
          , Show addrType
          , PrintfArg wordType
          , Show wordType
#endif
          )
       => addrType
       -- ^ Starting address where patch is applied
       -> Vector wordType
       -- ^ The patch
       -> MemorySystem addrType wordType
       -- ^ Memory system to which patch will be applied
       -> MemorySystem addrType wordType
       -- ^ Patched memory system
mPatch = doWrite False


-- | Write to memory: 'mPatch' and 'mWriteN' share this code.
doWrite :: ( Integral addrType
           , Integral wordType
           , Show wordType
           , DVU.Unbox wordType
#if defined(TEST_DEBUG)
           , PrintfArg addrType
           , Show addrType
           , PrintfArg wordType
           , Show wordType
#endif
           )
        => Bool
        -- ^ Preserve read-only regions: If True, prevents writing to 'ROMRegion's
        -> addrType
        -- ^ Starting address where patch is applied
        -> Vector wordType
        -- ^ The patch
        -> MemorySystem addrType wordType
        -- ^ Memory system to which patch will be applied
        -> MemorySystem addrType wordType
        -- ^ Patched memory system
doWrite readOnly startAddr patch msys = msys & regions %~ execState updRegionState
  where
    updMem = IM.mapAccumWithKey writeContents initState memRegions
    updRegionState = sequence [state (\m -> ((), IM.update (const (Just v)) k m)) | (k, v) <- (IM.assocs . snd) updMem]
    memRegions = IM.intersecting (msys ^. regions) (I.ClosedInterval startAddr (startAddr + fromIntegral (nWords - 1)))
    nWords = DVU.length patch
    initState = WriteState { curAddr = startAddr
                           , writeRemaining = nWords
                           , preserveRO = readOnly
                           , baseAddr = startAddr
                           , wordArray = patch
                           }


-- | Helper data type while new content is written to memory.
data WriteState addrType wordType where
  WriteState ::
    { curAddr :: addrType
    , writeRemaining :: Int
    -- Parameters from doWrite that are carried along
    , preserveRO :: Bool
    , baseAddr :: addrType
    , wordArray :: Vector wordType
    } -> WriteState addrType wordType


writeContents :: ( Integral addrType
                 , Integral wordType
                 , DVU.Unbox wordType
#if defined(TEST_DEBUG)
                 , PrintfArg addrType
                 , Show addrType
                 , PrintfArg wordType
                 , Show wordType
#endif
                 )
              => WriteState addrType wordType
              -> I.Interval addrType
              -> MemoryRegion addrType wordType
              -> (WriteState addrType wordType, MemoryRegion addrType wordType)
writeContents wstate iv mr
#if defined(TEST_DEBUG)
  | trace showProgress False
  = undefined
#endif
  | writeRemaining wstate <= 0
  = error ("doWrite " ++ (show . preserveRO) wstate ++ ": writeRemaining leq 0: " ++ (show . writeRemaining) wstate ++ "]")
  | saddr < sRegion
  -- Skip over unmapped memory here (TODO: Turn writes to unmapped memory into something.)
  = writeContents skipUnmappedRegion iv mr
  -- Don't molest ROM regions if the preserveRO flag is true. Otherwise, we can patch the ROM.
  | ROMRegion{} <- mr
  = ( updWriteState
    , if preserveRO wstate
      then mr
      else mr & contents .~ DVU.concat [leftSlice, patchSlice, rightSlice]
    )
  | DevRegion{} <- mr
  = ( updWriteState
    , DevRegion $ D.devWriteSeq (curAddr wstate - sRegion) wordVec (_rgnDevice mr)
    )
  -- If nWrite is greater than the LRU flush threshold, then clear out the pending writes and queue up the
  -- new updates.
  | nWrite < maxPending
  = ( updWriteState
    , execState writeQueueSeq mr
    )
  -- Cull all pending writes, update the underlying vector
  | otherwise
  = ( updWriteState
    , mr & writesPending %~ filterPending
         & contents .~ DVU.concat [ leftSlice, patchSlice, rightSlice ]
    )
  where
    saddr = curAddr wstate
    wordVec = wordArray wstate
    eRegion = I.upperBound iv
    sRegion = I.lowerBound iv
    sOffset = fromIntegral (saddr - sRegion)
    nWrite = min (fromIntegral (eRegion - saddr)) (writeRemaining wstate)
    updWriteState = wstate { curAddr        = saddr + fromIntegral nWrite
                           , writeRemaining = writeRemaining wstate - nWrite
                           }
    skipUnmappedRegion = wstate { curAddr = sRegion
                                , writeRemaining = writeRemaining wstate - fromIntegral (sRegion - saddr)
                                }
    writeQueueSeq = sequence [state (queueLRU (fromIntegral $ a - sRegion) w) | (a, w) <- zip addrWordsList patchAsList]
    addrWordsList = [fromIntegral offs + saddr | offs <- [0..nWrite]]
    patchAsList = DVU.toList $ DVU.slice (fromIntegral (saddr - baseAddr wstate)) nWrite wordVec

    -- You'd think something like this would exist in Prelude.
    posnum x
      | x >= 0
      = x
      | otherwise
      = 0

    leftLen = fromIntegral (posnum (saddr - sRegion))
    leftSlice =
      if leftLen > 0
      then views contents (DVU.slice 0 leftLen) mr
      else DVU.empty

    patchSlice =
      let patchOffset =
            if baseAddr wstate < saddr
            then fromIntegral (saddr - baseAddr wstate) + leftLen
            else 0
      in  DVU.slice patchOffset nWrite wordVec

    rightSlice =
      let rightOffs = sOffset + nWrite
          rightLen  = posnum (fromIntegral eRegion - rightOffs)
      in  if rightLen > 0
          then views contents (DVU.slice rightOffs rightLen) mr
          else DVU.empty

    filterPending lru = lru & lrucPsq %~ prunePsq [k | k <- views lrucPsq OrdPSQ.keys lru
                                                     , sOffset >= k && k < sOffset + nWrite]

#if defined(TEST_DEBUG)
    showProgress =
      printf "saddr 0x%04x sOffset %5d nWrite %5d"
        saddr
        sOffset
        nWrite
#endif


-- | Count the number of regions in the memory system; used primarily for testing and debugging
countRegions :: MemorySystem addrType wordType
             -> Int
countRegions msys = msys ^. regions . to IM.size

-- | Generate a list of the regions, without the contents. (Note: Does not permit changing the region's contents, primarily
-- intended as a test interface)
regionList :: ( Integral wordType
              , DVU.Unbox wordType
              )
           => MemorySystem addrType wordType
           -> [(I.Interval addrType, MemoryRegion addrType wordType)]
regionList msys = [ (i, r & contents .~ DVU.empty) | (i, r) <- IM.toList (msys ^. regions)]

-- | Sanity check the memory system's
sanityCheck :: MemorySystem addrType wordType
            -> Bool
sanityCheck msys =
  let verifyRegionSize reg = reg ^. writesPending . to sizeLRU == reg ^. nWritesPending
  in  all verifyRegionSize (msys ^. regions . to IM.elems)


-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed a given size.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Internal constants
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Pending write queue maximum depth
maxPending :: Int
maxPending = 71

-- | Number of pending writes to flush when 'maxPending' is reached
maxFlush :: Int
maxFlush = maxPending `div` 3

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed maxPending.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

-- | Create an empty write cache
initialLRU :: Int
         -- ^ Maximum size of the pending write cache
         -> LRUWriteCache wordType
         -- ^ The initial write cache
-- eta reduction: implied first argument, maxSize
initialLRU = LRUWriteCache 0 OrdPSQ.empty
{-# INLINE initialLRU #-}

-- | Calculate the size of the write cache
sizeLRU :: LRUWriteCache wordType
        -> Int
sizeLRU = (^. lrucPsq . to OrdPSQ.size)

lookupPendingWrite :: Int
                  -- ^ The address offset to probe
                  -> LRUWriteCache wordType
                  -- ^ The pending write cache
                  -> Maybe wordType
                  -- ^ The associated word value, if found, or 'Nothing'
lookupPendingWrite addr = views lrucPsq (fmap snd . OrdPSQ.lookup addr)
{-# INLINE lookupPendingWrite #-}

increaseTick :: Tick
             -> OrdPSQ.OrdPSQ Int Tick wordType
             -> Size
             -> LRUWriteCache wordType
increaseTick tick psq maxSize
  | tick < maxBound = LRUWriteCache (tick + 1) psq maxSize
  | otherwise       = retick psq OrdPSQ.empty 0
  where
    retick oldPsq newPsq newTick =
      case OrdPSQ.minView oldPsq of
        Nothing                 -> LRUWriteCache newTick newPsq maxSize
        Just (k, _, v, oldPsq') -> retick oldPsq' (OrdPSQ.insert k newTick v newPsq) (newTick + 1)

-- | Queue a write to 'addr' in the pending write cache, flushing the cache if its size exceeds 'maxPending'.
queueLRU :: ( Integral addrType
            , DVU.Unbox wordType
            , Integral wordType
            )
         => Int
         -- ^ Address offset into the memory region being modified
         -> wordType
         -- ^ New contents at the modified address
         -> MemoryRegion addrType wordType
         -- ^ Memory region to modify (_note_: flushing will modify '_contents')
         -> (wordType, MemoryRegion addrType wordType)
         -- ^ Previously written value, if the modified address was in the pending write queue
queueLRU addrOffs word mr =
  let LRUWriteCache nextTick psq maxSize = mr ^. writesPending
      (oldval, psq')                     = OrdPSQ.alter queueAddr addrOffs psq
      queueAddr Nothing                  = (Nothing, Just (nextTick, word))
      queueAddr (Just (_, oldword))      = (Just oldword, Just (nextTick, word))
      updateNWrites (Just _)             = 0
      updateNWrites Nothing              = 1
      doFlush mreg
        | mreg ^. nWritesPending >= maxPending
        = flushPending maxFlush mr'
        | otherwise
        = mreg
      mr'                            = mr & writesPending  .~ increaseTick (nextTick + 1) psq' maxSize
                                          & nWritesPending +~ updateNWrites oldval
      contentVal                     = (mr ^. contents) ! addrOffs
  in  (fromMaybe contentVal oldval, doFlush mr')

-- | Flush some of the pending writes to a memory region's content vector
flushPending :: ( Integral addrType
                , DVU.Unbox wordType
                , Integral wordType
                )
             => Int
             -- ^ Number of pending writes to commit
             -> MemoryRegion addrType wordType
             -- ^ Region where writes will be committed
             -> MemoryRegion addrType wordType
             -- ^ Updated region
flushPending nFlush mr =
  let -- Ascending list: from least recently to most recently used
      ents       = [(k, v) | (k, _, v) <- take nFlush (mr ^. writesPending . lrucPsq . to OrdPSQ.toAscList)]
      -- Bulk update the underlying contents, then prune the write queue (the power of Lenses!)
  in  mr & contents                %~ (// [(addrOffs, v) | (addrOffs, v) <- ents])
         & writesPending . lrucPsq %~ prunePsq (map fst ents)
         & nWritesPending          -~ length ents

prunePsq :: [Int]
         -- ^ Address offsets to prune
         -> OrdPSQ.OrdPSQ Int Tick wordType
         -- ^ Ordered priority queue to prune
         -> OrdPSQ.OrdPSQ Int Tick wordType
         -- ^ Resulting ordered priority queue
prunePsq addrOffsets psq = Fold.foldl' (flip OrdPSQ.delete) psq addrOffsets
{-# INLINE prunePsq #-}
