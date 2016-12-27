{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

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
  , mWrite
  , mWriteN
  , mPatch
  -- * Utility functions
  , countRegions
  , regionList
  , sanityCheck
  ) where

import           Control.Lens                    (Lens', (%~), (+~), (-~), (&), (.~), (^.), (|>), to, view, over)
import           Data.Monoid
import qualified Data.Foldable                   as Fold (foldl')
import qualified Data.OrdPSQ                     as OrdPSQ
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.IntervalMap.Interval       as I
import           Data.Vector.Unboxed             (Vector, (!), (//))
import qualified Data.Vector.Unboxed             as DVU
import           Data.Maybe (fromMaybe, isJust)
import           Prelude hiding (lookup)

-- import           Debug.Trace

import           Machine.ProgramCounter
import           Machine.Utils

-- | A memory region
data MemoryRegion addrType wordType where
  RAMRegion ::
    { _contents       :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    , _writesPending  :: LRUWriteCache addrType wordType
    -- ^ Pending write LRU cache
    , _nWritesPending :: {-# UNPACK #-} !Int
    -- ^ Size of the LRU cache (avoids the $O(n)$ penalty for calling 'OrdPSQ.size')
    } -> MemoryRegion addrType wordType
  ROMRegion ::
    { _contents :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  {-DevMemRegion :: (Monad m) =>
    { _contents       :: Vector wordType
    -- ^ Device memory's region's contents (unboxed vector)
    , _writesPending  :: LRUWriteCache addrType wordType
    -- ^ Pending write LRU cache
    , _nWritesPending :: {-# UNPACK #-} !Int
    -- ^ Size of the LRU cache (avoids the $O(n)$ penalty for calling 'OrdPSQ.size')
    , _readAction     :: m x
    -- ^ Monad action invoked before reading data
    , _writeAction    :: m x
    -- ^ Monad action invoked before writing data
    } -> MemoryRegion addrType wordType-}
  deriving (Show)

-- | Lens for a memory region's contents
contents :: Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion@(RAMRegion ctnt _ _) = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@(ROMRegion ctnt)     = (\content' -> mregion { _contents = content' }) <$> f ctnt

-- | Lens for the pending write LRU queue.
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache addrType wordType)
writesPending f mregion@RAMRegion{} = (\wp -> mregion { _writesPending = wp }) <$> f (_writesPending mregion)
-- For types other than 'RAMRegion', it's just the original memory region unchanged.
writesPending f mregion             = mregion <$ f (initialLRU 0)

-- | Lens for number of elements in the LRU queue
nWritesPending :: Lens' (MemoryRegion addrType wordType) Int
nWritesPending f mregion@RAMRegion{} = (\n -> mregion { _nWritesPending = n }) <$> f (_nWritesPending mregion)
nWritesPending f mregion             = mregion <$ f 0

-- | Pending write queue maximum depth
maxPending :: Int
maxPending = 71

-- | Number of pending writes to flush when 'maxPending' is reached
maxFlush :: Int
maxFlush = maxPending `div` 2

-- | Shorthand for the interval map of memory regions
type MemRegionMap addrType wordType = IM.IntervalMap (I.Interval addrType) (MemoryRegion addrType wordType)

-- | A memory system, for a given address type and word type.
--
-- NOTE: 'MemorySystem' is part of the 'Monoid' class, which makes it possible to merge memory systems together via 'mappend' or
-- 'mconcat', so long as the memory systems' regions do not overlap with each other. Regions in the merged memory system are distinct;
-- they are not coalesced.
data MemorySystem addrType wordType where
  MSys ::
    { _regions       :: MemRegionMap addrType wordType
    -- ^ Region interval map
    } -> MemorySystem addrType wordType
  deriving (Show)

-- Admit MemorySystem into the Monoid class
instance (Ord addrType) => Monoid (MemorySystem addrType wordType) where
  mempty              = initialMemorySystem
  msysA `mappend` msysB
    | IM.null (IM.intersection (msysA ^. regions) (msysB ^. regions))
    = over regions (mappend (msysB ^. regions )) msysA
    | otherwise
    = error "MemorySystem:mappend: Overlapping memory regions between memory systems"

-- | Lens for the memory region map inside a 'MemorySystem'
regions :: Lens' (MemorySystem addrType wordType) (MemRegionMap addrType wordType)
regions f msys = (\regions' -> msys { _regions = regions' }) <$> f (_regions msys)

-- | Create an empty memory system (alternately: 'mempty')
initialMemorySystem :: MemorySystem addrType wordType
initialMemorySystem = MSys { _regions = IM.empty }

-- | Create a new RAM memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkRAMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType, Num wordType) =>
               addrType
            -> Int
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkRAMRegion sa len msys =
  let ea        = sa + fromIntegral len
      newRegion = RAMRegion { _contents       = DVU.replicate len 0
                            , _writesPending  = initialLRU maxPending
                            , _nWritesPending = 0
                            }
  in    if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
        then over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys
        else error ("mkRAMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Create a new read-only (ROM) memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkROMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType) =>
               addrType
            -> Vector wordType
            -> MemorySystem addrType wordType
            -> MemorySystem addrType wordType
mkROMRegion sa romImg msys =
  let ea        = sa + fromIntegral (DVU.length romImg)
      newRegion = ROMRegion { _contents = romImg }
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned. (FIXME??)
mRead :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
          MemorySystem addrType wordType
       -> addrType
       -> wordType
mRead !msys !addr =
  let regs = IM.containing (msys ^. regions) addr
      getContent acc iv mr = acc |> fromMaybe (view contents mr ! fromIntegral (addr - I.lowerBound iv))
                                              (lookupPendingWrite addr (mr ^. writesPending))
      vals = IM.foldlWithKey getContent [] regs
      -- FIXME: Should something different happen here when data is requested from
      -- an unknown/unmapped region? Throw exception?
  in  if not (null vals) then head vals else 0

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: (Integral addrType, Num wordType, ShowHex addrType, DVU.Unbox wordType) =>
           MemorySystem addrType wordType
        -- ^ The memory system from which to read
        -> addrType
        -- ^ Starting address
        -> Int
        -- ^ Number of words to read
        -> Vector wordType
        -- ^ Contents read from memory
mReadN !msys !sa !nWords
  | nWords == 1
  = DVU.singleton (mRead msys sa)
  | otherwise
  = let regs                        = IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea)
        ea                          = sa + fromIntegral (nWords - 1)
        ((_, remain', accum), _)    = IM.mapAccumWithKey getContent (sa, nWords, ([] ++)) regs
        getContent (addr, remaining, vl) ivl reg
          {-  | trace ("addr = " ++ as0xHexS addr ++ " remaining = " ++ show remaining) False = undefined -}
          | addr >= lb  = let vl'    = (vl [DVU.slice (fromIntegral addr - fromIntegral lb) nb rcontent] ++)
                              nb     = min (fromIntegral ub - fromIntegral addr) remaining
                              accum' = (addr + fromIntegral nb, remaining - nb, vl')
                          in  (accum', reg)
          | addr < lb   = let flen   = fromIntegral (lb - addr)
                              nb'    = min (fromIntegral (ub - lb)) (remaining - flen)
                              nread  = nb' + flen
                              vl'    = (vl [DVU.replicate flen 0, DVU.slice 0 nb' rcontent] ++)
                              accum' = (addr + fromIntegral nread, remaining - nread, vl')
                          in  (accum', reg)
          -- Squelch GHC pattern warning...
          | otherwise   = error ("How'd I get here? addr = " ++ as0xHexS addr ++ " remaining " ++ show remaining)
          where
            lb       = I.lowerBound ivl
            ub       = I.upperBound ivl
            rcontent = reg ^. contents
        endfill                    = [DVU.replicate remain' 0]
        -- And ensure that pending writes in the regions are also included. This uses the same pattern that 'showS' uses,
        -- threading a list function across the regions so that concatenation only happens once and has linear performance.
        writes                = Fold.foldl' getWrites ([] ++) regs []
        getWrites pend reg    = (pend (reg ^. writesPending . lrucPsq . to filterWrites) ++)
        filterWrites psq      = [(a - sa, v) | (a, _, v) <- OrdPSQ.toList psq, a >= sa && a <= ea ]
        idxvec                = DVU.fromList (map (fromIntegral . fst) writes)
        valvec                = DVU.fromList (map snd writes)
    in  DVU.update_ ((DVU.concat . accum) endfill) idxvec valvec

-- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
-- pair.
mReadAndIncPC :: (PCOperation addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -- ^ Current program counter
               -> MemorySystem addrType wordType
               -- ^ The memory system from which the word will be fetched
               -> (ProgramCounter addrType, wordType)
               -- ^ Updated program counter and fetched word
mReadAndIncPC !pc !mem = (pc + 1, withPC pc (mRead mem))

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndRead :: (PCOperation addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
              -- ^ Current progracm counter
               -> MemorySystem addrType wordType
               -- ^ Memory system from which word will be fetched
               -> (ProgramCounter addrType, wordType)
               -- ^ Updated program counter and fetched word
mIncPCAndRead !pc !mem = let pc' = pc + 1
                         in  (pc', withPC pc' (mRead mem))

-- | Write a word into the memory system: inserts the word into the pending write LRU cache, flushing the cache to make
-- space if necessary, commiting updates to the underlying region's contents. If the memory region is not 'readWrite',
-- then nothing happens.
mWrite :: (Integral addrType, DVU.Unbox wordType) =>
          addrType
       -> wordType
       -> MemorySystem addrType wordType
       -> MemorySystem addrType wordType
mWrite !addr !word !msys =
  let regs                 = IM.keys (IM.intersecting (msys ^. regions) (I.ClosedInterval addr addr))
      writeRegion msys' iv = over regions (IM.update (queuePending (I.lowerBound iv)) iv) msys'
      -- Note: currently discarding the old value, (oldval, mr')
      queuePending lb mr@RAMRegion{} = Just (snd (queueLRU addr word lb mr))
      -- Otherwise, don't write anything.
      queuePending _lb mr            = Just mr
  in  Fold.foldl' writeRegion msys regs

-- | Write a block of memory, respecting the memory system's type
mWriteN :: (Integral addrType, DVU.Unbox wordType) =>
           addrType
        -- ^ Starting address
        -> Vector wordType
        -- ^ Content to write
        -> MemorySystem addrType wordType
        -- ^ The memory system to be written
        -> MemorySystem addrType wordType
        -- ^ Updated memory system
mWriteN = doWrite False

-- | Patch (forcibly overwrite) memory. This does not obey or check the 'readWrite' flag and will truncate the memory patch
-- if it extends beyond a region's upper bound.
mPatch :: (Integral addrType, DVU.Unbox wordType) =>
          addrType
          -- ^ Starting address where patch is applied
       -> Vector wordType
          -- ^ The patch
       -> MemorySystem addrType wordType
          -- ^ Memory system to which patch will be applied
       -> MemorySystem addrType wordType
          -- ^ Patched memory system
mPatch = doWrite True

-- | Write to memory: 'mPatch' and 'mWriteN' share this code.
doWrite :: (Integral addrType, DVU.Unbox wordType) =>
           Bool
        -- ^ Force write flag: If True, writes are permitted in 'ROMRegion's
        -> addrType
           -- ^ Starting address where patch is applied
        -> Vector wordType
           -- ^ The patch
        -> MemorySystem addrType wordType
           -- ^ Memory system to which patch will be applied
        -> MemorySystem addrType wordType
           -- ^ Patched memory system
doWrite !forceWrite !paddr !patch !msys =
  let minterval                = I.ClosedInterval paddr ea
      ea                       = paddr + fromIntegral (DVU.length patch - 1)
      (_, mregs)               = IM.mapAccumWithKey updContent paddr (msys ^. regions)
      updContent sa iv reg
        -- Don't molest ROM regions if forced writes aren't in effect.
        | (ROMRegion _) <- reg
        , not forceWrite
        = (ub + 1, reg)
        | iv `I.overlaps` minterval
        = (ub + 1, reg & contents       %~ (\vec -> DVU.update_ vec pidxs pslice)
                       & writesPending  .~ culledPending
                       & nWritesPending -~ nCulled)
        | otherwise
        = (ub + 1, reg)
        where
          lb                       = I.lowerBound iv
          ub                       = I.upperBound iv
          sa'                      = max sa lb
          sidx                     = fromIntegral sa' - fromIntegral paddr
          plen                     = min (DVU.length patch - sidx) (fromIntegral ub - fromIntegral sa')
          idxoffs                  = fromIntegral sa' - fromIntegral lb
          pslice                   = DVU.slice sidx plen patch
          pidxs                    = DVU.generate plen (+ idxoffs)
          (nCulled, culledPending) = cullPendingWrites sa ea (reg ^. writesPending)
  in  msys & regions .~ mregs

-- Delete all pending writes in a region, but don't commit any values.
cullPendingWrites :: (Ord addrType) =>
                     addrType
                  -> addrType
                  -> LRUWriteCache addrType wordType
                  -> (Int, LRUWriteCache addrType wordType)
cullPendingWrites sa ea wp  = (length addrs, over lrucPsq (prunePsq addrs) wp)
  where
    addrs            = filter addrInRange [a | (a, _, _) <- OrdPSQ.toList (wp ^. lrucPsq)]
    addrInRange addr = addr >= sa && addr <= ea

-- | Count the number of regions in the memory system; used primarily for testing and debugging
countRegions :: MemorySystem addrType wordType
             -> Int
countRegions msys = msys ^. regions . to IM.size

-- | Generate a list of the regions, without the contents. (Note: Does not permit changing the region's contents, primarily
-- intended as a test interface)
regionList :: (DVU.Unbox wordType) =>
              MemorySystem addrType wordType
           -> [(I.Interval addrType, MemoryRegion addrType wordType)]
regionList msys = [ (i, r & contents .~ DVU.empty) | (i, r) <- IM.toList (msys ^. regions)]

-- | Sanity check the memory system's 
sanityCheck :: MemorySystem addrType wordType
            -> Bool
sanityCheck msys =
  let verifyRegionSize reg = reg ^. writesPending . to sizeLRU == reg ^. nWritesPending
  in  all verifyRegionSize (IM.elems (view regions msys))

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- LRU pending write cache implementation, adapted from the psqueues example source. The pending write cache will
-- not exceed a given size.
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

type Tick = Int
type Size = Int

data LRUWriteCache addrType wordType where
  LRUWriteCache ::
    { _lrucNextTick :: {-# UNPACK #-} !Tick
    , _lrucPsq      ::                !(OrdPSQ.OrdPSQ addrType Tick wordType)
    , _lrucMaxSize  :: {-# UNPACK #-} !Size
    } -> LRUWriteCache addrType wordType
    deriving (Show)

lrucPsq :: Lens' (LRUWriteCache addrType wordType) (OrdPSQ.OrdPSQ addrType Tick wordType)
lrucPsq f psq = (\psq' -> psq { _lrucPsq = psq' }) <$> f (_lrucPsq psq)

-- | Create an empty write cache
initialLRU :: Int
         -- ^ Maximum size of the pending write cache
         -> LRUWriteCache addrType wordType
         -- ^ The initial write cache
-- eta reduction: implied first argument, maxSize
initialLRU = LRUWriteCache 0 OrdPSQ.empty
{-# INLINE initialLRU #-}

-- | Calculate the size of the write cache
sizeLRU :: LRUWriteCache addrType wordType
        -> Int
sizeLRU = (^. lrucPsq . to OrdPSQ.size)
{-# INLINE sizeLRU #-}

lookupPendingWrite :: (Ord addrType) =>
                      addrType
                  -- ^ The address to probe
                  -> LRUWriteCache addrType wordType
                  -- ^ The pending write cache
                  -> Maybe wordType
                  -- ^ The associated word value, if found, or 'Nothing'
lookupPendingWrite !addr = (^. lrucPsq . to (fmap snd . OrdPSQ.lookup addr))
{-# INLINE lookupPendingWrite #-}

increaseTick :: (Ord addrType) =>
                Tick
             -> OrdPSQ.OrdPSQ addrType Tick wordType
             -> Size
             -> LRUWriteCache addrType wordType
increaseTick tick psq maxSize
  | tick < maxBound = LRUWriteCache (tick + 1) psq maxSize
  | otherwise       = retick psq OrdPSQ.empty 0
  where
    retick !oldPsq !newPsq !newTick =
      case OrdPSQ.minView oldPsq of
        Nothing                 -> LRUWriteCache newTick newPsq maxSize
        Just (k, _, v, oldPsq') -> retick oldPsq' (OrdPSQ.insert k newTick v newPsq) (newTick + 1)

-- | Queue a write to 'addr' in the pending write cache, flushing the cache if its size exceeds 'maxPending'.
queueLRU :: (Integral addrType, DVU.Unbox wordType) =>
            addrType
         -- ^ Address being modified
         -> wordType
         -- ^ New contents at the modified address
         -> addrType
         -- ^ Memory region's starting address
         -> MemoryRegion addrType wordType
         -- ^ Memory region to modify (_note_: flushing will modify '_contents')
         -> (wordType, MemoryRegion addrType wordType)
         -- ^ Previously written value, if the modified address was in the pending write queue
queueLRU !addr !word !sa !mr =
  let LRUWriteCache nextTick psq maxSize = mr ^. writesPending
      (oldval, psq')                     = OrdPSQ.alter queueAddr addr psq
      queueAddr Nothing                  = (Nothing, Just (nextTick, word))
      queueAddr (Just (_, oldword))      = (Just oldword, Just (nextTick, word))
      mr'                                = mr & writesPending  .~ increaseTick (nextTick + 1) psq' maxSize
                                              & nWritesPending +~ (if isJust oldval then 0 else 1)
      mr''                               = if   mr' ^. nWritesPending >= maxPending
                                           then flushPending maxFlush sa mr'
                                           else mr'
      contentVal                         = (mr ^. contents) ! fromIntegral (addr - sa)
  in  (fromMaybe contentVal oldval, mr'')

-- | Flush some of the pending writes to a memory region's content vector
flushPending :: (Integral addrType, DVU.Unbox wordType) =>
                Int
             -- ^ Number of pending writes to commit
             -> addrType
             -- ^ Start address of the region
             -> MemoryRegion addrType wordType
             -- ^ Region where writes will be committed
             -> MemoryRegion addrType wordType
             -- ^ Updated region
flushPending nFlush sa mr =
  let -- Ascending list: from least recently to most recently used
      ents       = [(k, v) | (k, _, v) <- take nFlush (mr ^. writesPending . lrucPsq . to OrdPSQ.toAscList)]
      upds       = [(fromIntegral (addr - sa), v) | (addr, v) <- ents]
      -- Bulk update the underlying contents, then prune the write queue (the power of Lenses!)
  in  mr & contents                %~ (// upds)
         & writesPending . lrucPsq %~ prunePsq (map fst ents)
         & nWritesPending          -~ length ents

prunePsq :: (Ord addrType) =>
            [addrType]
         -> OrdPSQ.OrdPSQ addrType Tick wordType
         -> OrdPSQ.OrdPSQ addrType Tick wordType
prunePsq addrs psq = Fold.foldl' (flip OrdPSQ.delete) psq addrs
{-# INLINE prunePsq #-}
