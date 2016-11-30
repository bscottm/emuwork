{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- OPTIONS_HADDOCK ignore-exports -}
{-| Representation of system memory.

Emulated system memory is a collection of regions stored in an 'IntervalMap'. The 'IntervalMap' stores the
start and end addresses for the region; each region stores its contents and a read-only flag. Memory regions __/may not/__
overlap; they __/must/__ be distinct.

Writing to a 'MemorySystem' places the modified word for an address into a priority search queue that acts as a LRU cache.
This amortizes the penalty from updating the underlying vector holding the actual contents until the queue's size is
exceeded (currently 29 elements.)

How to use: Add regions to an 'initialMemorySystem' via 'mkRAMRegion' or 'mkROMRegion'. Example snippet from the TRS-80
code:

> -- | Create the system's RAM
> installMem :: TRS80ModelISystem
>            -> Int
>            -> Vector Z80word
>            -> TRS80ModelISystem
> installMem sys memSize newROM =
>   sys & memory %~ mkROMRegion 0 newROM & memory %~ mkRAMRegion ramStart (memSize * 1024)
 -}

module Machine.MemorySystem
  ( -- * Data Types and Accessors
    MemorySystem(..)
  -- * Initialization
  , initialMemorySystem
  -- * Memory Regions
  , mkRAMRegion
  , mkROMRegion
  -- * Read Memory
  , mRead
  , mReadN
  , mReadAndIncPC
  , mIncPCAndRead
  -- * Write Memory
  , mPatch
  -- * Utility functions
  , countRegions
  , regionList
  ) where

import           Control.Lens                    (Lens', (%~), (&), (.~), (^.),
                                                  (|>))
import           Data.Hashable                   (Hashable)
import qualified Data.HashPSQ                    as HashPSQ
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.IntervalMap.Interval       as I
import           Data.Vector.Unboxed             (Vector, (!))
import qualified Data.Vector.Unboxed             as DVU
import           Data.Maybe (fromMaybe)
import           Prelude hiding (lookup)

import           Machine.ProgramCounter
import           Machine.Utils

-- | A memory region
data MemoryRegion addrType wordType where
  MR ::
    { _readWrite   :: Bool
    -- ^ Read/write flag for the reguion: If 'True', region can be written. Otherwise, it's read-only (i.e., ROM)
    , _contents    :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    , _writesPending :: LRUWriteCache addrType wordType
    -- ^ Pending write LRU cache
    } -> MemoryRegion addrType wordType
  deriving (Show)

-- | Lens for a memory region's read-only flag.
readWrite :: Lens' (MemoryRegion addrType wordType) Bool
readWrite f mregion = (\ro -> mregion { _readWrite = ro }) <$> f (_readWrite mregion)

-- | Lens for a memory region's contents
contents :: Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion = (\content' -> mregion { _contents = content' }) <$> f (_contents mregion)

-- | Lens for the pending write LRU queue
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache addrType wordType)
writesPending f mreg = (\wp -> mreg { _writesPending = wp }) <$> f (_writesPending mreg)

-- | Pending write queue maximum depth
maxPending :: Int
maxPending = 29

-- | Shorthand for the interval map of memory regions
type MemRegionMap addrType wordType = IM.IntervalMap (I.Interval addrType) (MemoryRegion addrType wordType)

-- | A memory system, for a given address type and word type.
data MemorySystem addrType wordType where
  MSys ::
    { _regions       :: MemRegionMap addrType wordType
    -- ^ Region interval map
    } -> MemorySystem addrType wordType

-- | Lens for the memory region map inside a 'MemorySystem'
regions :: Lens' (MemorySystem addrType wordType) (MemRegionMap addrType wordType)
regions f msys = (\regions' -> msys { _regions = regions' }) <$> f (_regions msys)

-- | Create an empty memory system
initialMemorySystem :: MemorySystem addrType wordType
initialMemorySystem = MSys { _regions = IM.empty
                           }

-- | Create a new RAM memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkRAMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType, Num wordType) =>
               addrType
            -> Int
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkRAMRegion sa len msys =
  let ea = sa + fromIntegral len
  in    if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
        then msys & regions %~ IM.insertWith const
                                             (I.IntervalCO sa ea)
                                             MR { _readWrite = True
                                                , _contents = DVU.replicate len 0
                                                , _writesPending = emptyLRU maxPending
                                                }
        else error ("mkRAMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Create a new read-only (ROM) memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkROMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType) =>
               addrType
            -> Vector wordType
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkROMRegion sa romImg msys =
  let ea = sa + fromIntegral (DVU.length romImg)
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then msys & regions %~ IM.insertWith const
                                           (I.IntervalCO sa ea)
                                           MR { _readWrite = False
                                              , _contents = romImg
                                              , _writesPending = emptyLRU maxPending
                                              }
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned. (FIXME??)
mRead :: (Integral addrType, Hashable addrType, Num wordType, DVU.Unbox wordType) =>
          MemorySystem addrType wordType
       -> addrType
       -> wordType
mRead msys addr =
  let regs = IM.containing (msys ^. regions) addr
      getContent acc iv mr = acc |> fromMaybe ((mr ^. contents) ! fromIntegral (addr - I.lowerBound iv))
                                              (lookupPendingWrite addr (mr ^. writesPending))
      vals = IM.foldlWithKey getContent [] regs
      -- FIXME: Should something different happen here when data is requested from
      -- an unknown/unmapped region? Throw exception?
  in  if not (null vals) then head vals else 0

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: (Integral addrType, Num wordType, ShowHex addrType, DVU.Unbox wordType) =>
           MemorySystem addrType wordType
        -> addrType
        -> Int
        -> Vector wordType
mReadN msys sa nWords =
  let regs          = IM.intersecting (msys ^. regions) (I.ClosedInterval sa (sa + fromIntegral nWords))
      go (addr, remaining, vl) ivl reg
        | addr > lb  = let nb     = min (ub - addr) (fromIntegral remaining)
                           vl'    = (vl [DVU.slice (fromIntegral (addr - lb)) (fromIntegral nb) cts] ++)
                           accum' = (ub, remaining - fromIntegral nb, vl')
                       in  (accum', reg)
        | addr == lb = let nb = min (ub - addr) (fromIntegral remaining)
                           accum' = (ub - nb, remaining - fromIntegral nb, (vl [DVU.slice 0 (fromIntegral nb) cts] ++))
                       in  (accum', reg)
        | addr < lb  = let nb     = min (ub - addr) (fromIntegral remaining)
                           vl'    = (vl [DVU.replicate (fromIntegral (lb - addr)) 0, DVU.slice 0 (fromIntegral nb) cts] ++)
                           accum' = (ub - nb, remaining - fromIntegral nb, vl')
                       in  (accum', reg)
        -- Squelch GHC pattern warning...
        | otherwise   = error ("How'd I get here? addr = " ++ as0xHexS addr ++ " remaining " ++ show remaining)
        where
          lb  = I.lowerBound ivl
          ub  = I.upperBound ivl
          cts = reg ^. contents
      ((_, remain', accum), _)   = IM.mapAccumWithKey go (sa, nWords, ([] ++)) regs
      endfill                    = [DVU.replicate remain' 0]
  in  DVU.concat (accum endfill)

-- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
-- pair.
mReadAndIncPC :: (PCOperation addrType, Hashable addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -- ^ Current program counter
               -> MemorySystem addrType wordType
               -- ^ The memory system from which the word will be fetched
               -> (ProgramCounter addrType, wordType)
               -- ^ Updated program counter and fetched word

mReadAndIncPC pc mem = (pc + 1, withPC pc (mRead mem))

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndRead :: (PCOperation addrType, Hashable addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
              -- ^ Current progracm counter
               -> MemorySystem addrType wordType
               -- ^ Memory system from which word will be fetched
               -> (ProgramCounter addrType, wordType)
               -- ^ Updated program counter and fetched word
mIncPCAndRead pc mem = let pc' = pc + 1
                       in  (pc', withPC pc' (mRead mem))

-- | Write a word into the memory system: inserts the word into the pending write LRU cache, flushing the cache to make
-- space if necessary, commiting updates to the underlying region's contents.
mWrite :: addrType
       -> wordType
       -> MemorySystem addrType wordType
       -> MemorySystem addrType wordType
mWrite addr word msys =
  let regs          = IM.intersecting (msys ^. regions) (I.ClosedInterval addr addr)
  in  undefined
  
-- | Patch (forcibly overwrite) memory. This does not obey or check the 'readWrite' flag and will truncate the memory patch
-- if it extends beyond a region's upper bound.
mPatch :: (Ord addrType, Num addrType, DVU.Unbox wordType) =>
          addrType
          -- ^ Starting address where patch is applied
       -> Vector wordType
          -- ^ The patch
       -> MemorySystem addrType wordType
          -- ^ Memory system to which patch will be applied
       -> MemorySystem addrType wordType
          -- ^ Patched memory system
mPatch paddr patch msys =
  let minterval     = I.ClosedInterval paddr (paddr + fromIntegral (DVU.length patch) - 1)
      mregs         = IM.intersecting (msys ^. regions) minterval
  in  undefined

-- | Count the number of regions in the memory system; used primarily for testing and debugging
countRegions :: MemorySystem addrType wordType
             -> Int
countRegions msys = IM.size (msys ^. regions)

-- | Generate a list of the regions, without the contents. (Note: Does not permit changing the region's contents, primarily
-- intended as a test interface)
regionList :: (DVU.Unbox wordType) =>
              MemorySystem addrType wordType
           -> [(I.Interval addrType, MemoryRegion addrType wordType)]
regionList msys = [ (i, r & contents .~ DVU.empty) | (i, r) <- IM.toList (msys ^. regions)]

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed a given size.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

type Tick = Int
type Size = Int

data LRUWriteCache addrType wordType where
  LRUWriteCache ::
    { lrucNextTick :: {-# UNPACK #-} !Tick
    , lrucPsq      ::                !(HashPSQ.HashPSQ addrType Tick wordType)
    , lrucMaxSize  :: {-# UNPACK #-} !Size
    } -> LRUWriteCache addrType wordType
    deriving (Show)
-- | Create an empty write cache
emptyLRU :: Int
         -- ^ Maximum size of the pending write cache
         -> LRUWriteCache addrType wordType
         -- ^ The initial write cache
emptyLRU = LRUWriteCache 0 HashPSQ.empty
{-# INLINE emptyLRU #-}

-- | Calculate the size of the write cache
sizeLRU :: LRUWriteCache addrType wordType
        -> Int
sizeLRU = HashPSQ.size . lrucPsq
{-# INLINE sizeLRU #-}

{- NOTUSED? don't need to update the LRU tick if just looking up
lookupLRU :: addrType
          -> LRUWriteCache addrType wordType
          -> (Maybe v, LRUWriteCache addrType wordType)
lookupLRU addr cache@(LRUWriteCache nextTick psq maxSize) =
    case HashPSQ.alter tickleIfExists addr psq of
      (Nothing,       _   ) -> (Nothing, cache)
      (mbV@(Just _),  psq') -> (mbV,     increaseTick nextTick psq' maxSize)
  where
    tickleIfExists Nothing       = (Nothing, Nothing)
    tickleIfExists (Just (_, v)) = (Just v,  Just (nextTick, v))
-}

lookupPendingWrite :: (Ord addrType, Hashable addrType) =>
                      addrType
                  -- ^ The address to probe
                  -> LRUWriteCache addrType wordType
                  -- ^ The pending write cache
                  -> Maybe wordType
                  -- ^ The associated word value, if found, or 'Nothing'
lookupPendingWrite addr = fmap snd . HashPSQ.lookup addr . lrucPsq
{-# INLINE lookupPendingWrite #-}

increaseTick :: (Ord addrType, Hashable addrType) =>
                Tick
             -> HashPSQ.HashPSQ addrType Tick wordType
             -> Size
             -> LRUWriteCache addrType wordType
increaseTick tick psq maxSize
  | tick < maxBound = LRUWriteCache (tick + 1) psq maxSize
  | otherwise       = retick psq HashPSQ.empty 0
  where
    retick !oldPsq !newPsq !newTick =
      case HashPSQ.minView oldPsq of
        Nothing                 -> LRUWriteCache newTick newPsq maxSize
        Just (k, _, v, oldPsq') -> retick oldPsq' (HashPSQ.insert k newTick v newPsq) (newTick + 1)

insertLRU :: (Ord addrType, Hashable addrType) =>
             addrType
          -- ^ The address
          -> wordType
          -- ^ The word to write at the address
          -> LRUWriteCache addrType wordType
          -- ^ Incoming pending write cache
          -> (LRUWriteCache addrType wordType, Maybe (addrType, wordType))
insertLRU addr word (LRUWriteCache nextTick psq maxSize)
    | HashPSQ.size psq' <= maxSize
    = (increaseTick nextTick psq' maxSize, Nothing)
    | otherwise
    = fromMaybe (emptyLRU maxSize, Nothing) $ do
        (addr', _, word', psq'') <- HashPSQ.minView psq'
        return (increaseTick nextTick psq'' maxSize, Just (addr', word'))
    where
      psq' = HashPSQ.insert addr nextTick word psq

-- | Evict the entry at the given key in the cache.
delete :: (Ord addrType, Hashable addrType) =>
           addrType
        -> LRUWriteCache addrType wordType
        -> (LRUWriteCache addrType wordType, Maybe wordType)
delete addr cache = case HashPSQ.deleteView addr (lrucPsq cache) of
    Nothing               -> (cache,                  Nothing)
    Just (_t, word, psq') -> (cache {lrucPsq = psq'}, Just word)

{-
deleteLRU :: LRUWriteCache addrType wordType -> (LRUWriteCache addrType wordType, Maybe (Int, v))
deleteLRU (LRUWriteCache nextTick psq maxSize) =
    fromMaybe (emptyLRU maxSize, Nothing) $ do
        (k, _, v, psq') <- HashPSQ.minView psq
        return (LRUWriteCache nextTick psq' maxSize, Just (k, v))
-}
