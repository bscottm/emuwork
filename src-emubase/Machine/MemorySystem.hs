{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

{-| General-purpose system memory, for both traditional memory and for port-based I/O systems.

Memory is a collection of address regions ('addrType') stored in an 'IntervalMap'. The 'IntervalMap' stores the start
and end addresses for the region; each region may hold a `RAMRegion` (random access memory), a `ROMRegion` (read-only
memory) or an index to a device (`DevRegion`). Both RAM and ROM regions store content as vectors of a `wordType`.
Devices manage content as they see fit, although they may only return or store a `wordType`-sized item. Memory regions
__/may not/__ overlap; they __/must/__ be distinct.

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
  ( -- * Data Types and Accessors
    MemorySystem(..)
  , IOSystem
  -- * Initialization
  , initialMemorySystem
  -- * Memory-mapped devices
  , deviceTable
  , nDevices
  -- * Memory Regions
  , mkRAMRegion
  , mkROMRegion
  , mkDevRegion
  -- * Read Memory
  , MemRead
  , MemReadN
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

import           Control.Arrow                   (first, (***))
import           Control.Lens                    (Lens', over, to, view, (%~), (&), (+~), (-~), (.~), (^.), (|>))
import qualified Data.Foldable                   as Fold (foldl')
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as H
import qualified Data.IntervalMap.Generic.Strict as IM
import qualified Data.IntervalMap.Interval       as I
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import qualified Data.OrdPSQ                     as OrdPSQ
import           Data.Vector.Unboxed             (Vector, (!), (//))
import qualified Data.Vector.Unboxed             as DVU
import           Prelude                         hiding (lookup)

-- import           Debug.Trace

import           Machine.Device
import           Machine.ProgramCounter
import           Machine.Utils

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Shorthand for the memory region interval map
type MemRegionMap addrType wordType = IM.IntervalMap (I.Interval addrType) (MemoryRegion addrType wordType)
-- | Shorthand for the device manager
type DeviceManager addrType wordType = HashMap Int (Device addrType wordType)

{- | `MemorySystem` provides a unified interface to an emulated memory system: RAM, ROM and devices that occupy memory
address ranges. Note, though, that port-based I/O is also a type of memory system (see the `IOSystem` type alias), where
the addresses are port numbers.

Devices change state when read or written. Consequently, `MemorySystem` has a device table to maintain consistent device
state, which is updated after a read or a write. Reads and writes all return the updated memory system, with which the
caller has to use or reuse on subsequent memory system reads and writes.

`MemorySystem` is also part of the `Monoid` class, which makes it possible to merge memory systems together via
'mappend' or -- 'mconcat', so long as the memory systems' regions do not overlap with each other. Regions in the merged
memory system are distinct; they are not coalesced.
-}
data MemorySystem addrType wordType where
  MSys ::
    { _regions    :: MemRegionMap addrType wordType
    -- ^ Region interval map
    , _devices    :: DeviceManager addrType wordType
    -- ^ Container for memory-mapped devices, indexed by an integer identifier
    , _nDevices   :: Int
    -- ^ Number of devices (and device identifier generator)
    } -> MemorySystem addrType wordType

instance (Show addrType, Show wordType, DVU.Unbox wordType) =>
         Show (MemorySystem addrType wordType) where
  show msys = "MemorySystem " ++ show (msys ^. regions) ++ " " ++ show (msys ^. nDevices)

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

-- | Lens for the memory-mapped device container
deviceTable :: Lens' (MemorySystem addrType wordType) (DeviceManager addrType wordType)
deviceTable f msys = (\memD -> msys { _devices = memD }) <$> f (_devices msys)

-- | Lens for the memory-mapped device container
nDevices :: Lens' (MemorySystem addrType wordType) Int
nDevices f msys = (\n -> msys { _nDevices = n }) <$> f (_nDevices msys)

-- | Create an empty memory system (alternately: 'mempty')
initialMemorySystem :: MemorySystem addrType wordType
initialMemorySystem = MSys { _regions  = IM.empty
                           , _devices  = H.empty
                           , _nDevices = 0
                           }

-- | I/O system based on ports, e.g., Zilog and Intel, are an address space of their own, which makes
-- a type alias for `MemorySystem` appropriate. Port-based I/O systems use ports for their address space and this
-- results in a better code re-use.
type IOSystem portType wordType = MemorySystem portType wordType

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

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
    { _contents       :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  DevMemRegion :: Int
               -- Index into the memory system's '_memDevices' memory-mapped device container
               -> MemoryRegion addrType wordType
  deriving (Show)

-- | Lens for a memory region's contents
contents :: (DVU.Unbox wordType) =>
            Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion@(RAMRegion ctnt _ _) = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@(ROMRegion ctnt)     = (\content' -> mregion { _contents = content' }) <$> f ctnt
contents f mregion@DevMemRegion{}       = mregion <$ f DVU.empty

-- | Lens for the pending write LRU queue.
writesPending :: Lens' (MemoryRegion addrType wordType) (LRUWriteCache addrType wordType)
writesPending f mregion@RAMRegion{}    = (\wp -> mregion { _writesPending = wp }) <$> f (_writesPending mregion)
-- For types other than 'RAMRegion', it's just the original memory region unchanged.
writesPending f mregion@ROMRegion{}    = mregion <$ f (initialLRU 0)
writesPending f mregion@DevMemRegion{} = mregion <$ f (initialLRU 0)

-- | Lens for number of elements in the LRU queue
nWritesPending :: Lens' (MemoryRegion addrType wordType) Int
nWritesPending f mregion@RAMRegion{} = (\n -> mregion { _nWritesPending = n }) <$> f (_nWritesPending mregion)
nWritesPending f mregion             = mregion <$ f 0

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Internal constants
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Pending write queue maximum depth
maxPending :: Int
maxPending = 71

-- | Number of pending writes to flush when 'maxPending' is reached
maxFlush :: Int
maxFlush = maxPending `div` 2

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Create a new RAM memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkRAMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType, Num wordType) =>
               addrType
            -- ^ RAM region start address
            -> Int
            -- ^ Length of the RAM region
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
            -- ^ Region's start address
            -> Vector wordType
            -- ^ ROM image to insert as the region's contents
            -> MemorySystem addrType wordType
            -- ^ Memory system to alter
            -> MemorySystem addrType wordType
            -- ^ Resulting memory system with the inserted ROM region
mkROMRegion sa romImg msys =
  let ea        = sa + fromIntegral (DVU.length romImg)
      newRegion = ROMRegion { _contents = romImg }
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Create a new region for a memory-mapped device.
mkDevRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType) =>
               addrType
            -- ^ Start address of the region
            -> addrType
            -- ^ End address of the region
            -> Device addrType wordType
            -- ^ The device type
            -> MemorySystem addrType wordType
            -- ^ Memory region to augment
            -> (Int, MemorySystem addrType wordType)
            -- ^ Resulting memory system with the device region inserted
mkDevRegion sa ea dev msys =
  let devIdx = msys ^. nDevices
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then (devIdx, msys & nDevices     +~ 1
                         & regions      %~ IM.insertWith const (I.IntervalCO sa ea) (DevMemRegion devIdx)
                         & deviceTable  %~ H.insert devIdx dev)
      else error ("mkDevRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Read/write memory
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Return type for 'mRead'. It has to be this way because devices __can__ (and usually __do__) change
-- state when read, producing an altered 'MemorySystem'
type MemRead addrType wordType = (wordType, MemorySystem addrType wordType)
-- | Return type for 'mReadN'. Devices __can__ and usually __do__ change state when read, producing an altered
-- 'MemorySystem'.
type MemReadN addrType wordType = (Vector wordType, MemorySystem addrType wordType)

-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned.
mRead :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
         addrType
      -- ^ Address to read
      -> MemorySystem addrType wordType
      -- ^ Memory system, current state
      -> MemRead addrType wordType
      -- ^ Memory system, resulting state
mRead !addr !msys =
  let -- Cute use of arrows to operate on the pair returned by 'devRead':
      getContent (vals', msys') _iv (DevMemRegion devIdx) =
        let doMemRead dev' = (vals' |>) *** doDevUpd $ deviceRead addr dev'
            doDevUpd  dev' = msys' & deviceTable %~ H.update (\_ -> Just dev') devIdx
            defaultVal     = (vals' |> 0, msys')
        in  maybe defaultVal doMemRead (H.lookup devIdx (msys' ^. deviceTable))
      getContent (vals', msys') iv mr =
        let memval = fromMaybe (view contents mr ! fromIntegral (addr - I.lowerBound iv))
                               (lookupPendingWrite addr (mr ^. writesPending))
        in (vals' |> memval, msys')
      -- FIXME: Need another function that throws an exception (polluting everything with the IO monad) or otherwise signals
      -- that the read was from unmapped memory. Or: Provide a function that checks the address is mapped, let that throw
      -- an exception, otherwise return the value.
      --
      -- Here, just return 0 if no value could be read from the memory regions.
      ensureValue []   = 0
      ensureValue vals = head vals
      -- And another use of an arrow on a pair.
  in  first ensureValue (IM.foldlWithKey getContent ([], msys) (IM.containing (msys ^. regions) addr))

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: (Integral addrType, Num wordType, ShowHex addrType, DVU.Unbox wordType) =>
          addrType
       -- ^ Starting address
       -> Int
       -- ^ Number of words to read
       -> MemorySystem addrType wordType
       -- ^ The memory system from which to read
       -> MemReadN addrType wordType
       -- ^ Contents read from memory
mReadN !sa !nWords !msys
  {- Trivial optimization: Could also do this for other small numbers, e.g., nWords <= 4 -}
  | nWords == 1
  = first DVU.singleton (mRead sa msys)
  | otherwise
  = let memRegions                           = IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea)
        ea                                   = sa + fromIntegral (nWords - 1)
        ((_, remain', accum, resultMsys), _) = IM.mapAccumWithKey getContent (sa, nWords, ([] ++), msys) memRegions
        getContent (addr, remaining, vl, msys') ivl reg
          {-  | trace ("addr = " ++ as0xHexS addr ++ " remaining = " ++ show remaining) False = undefined -}
          | DevMemRegion _devIdx <- reg
          = undefined
          {- Other regions must have actual content -}
          | addr >= lb  = let vl'    = (vl [DVU.slice (fromIntegral addr - fromIntegral lb) nb rcontent] ++)
                              nb     = min (fromIntegral ub - fromIntegral addr) remaining
                              accum' = (addr + fromIntegral nb, remaining - nb, vl', msys')
                          in  (accum', reg)
          | addr < lb   = let flen   = fromIntegral (lb - addr)
                              nb'    = min (fromIntegral (ub - lb)) (remaining - flen)
                              nread  = nb' + flen
                              vl'    = (vl [DVU.replicate flen 0, DVU.slice 0 nb' rcontent] ++)
                              accum' = (addr + fromIntegral nread, remaining - nread, vl', msys')
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
        writes                = Fold.foldl' getWrites ([] ++) memRegions []
        getWrites pend reg    = (pend (reg ^. writesPending . lrucPsq . to filterWrites) ++)
        filterWrites psq      = [(a - sa, v) | (a, _, v) <- OrdPSQ.toList psq, a >= sa && a <= ea ]
        idxvec                = DVU.fromList (map (fromIntegral . fst) writes)
        valvec                = DVU.fromList (map snd writes)
    in  (DVU.update_ ((DVU.concat . accum) endfill) idxvec valvec, resultMsys)

-- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
-- pair.
mReadAndIncPC :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -- ^ Current program counter
               -> MemorySystem addrType wordType
               -- ^ The memory system from which the word will be fetched
               -> (ProgramCounter addrType, MemRead addrType wordType)
               -- ^ Updated program counter and fetched word
mReadAndIncPC !pc !mem = (pc + 1, mRead (unPC pc) mem)

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndRead :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
              -- ^ Current progracm counter
               -> MemorySystem addrType wordType
               -- ^ Memory system from which word will be fetched
               -> (ProgramCounter addrType, MemRead addrType wordType)
               -- ^ Updated program counter and fetched word
mIncPCAndRead !pc !mem = let pc' = pc + 1
                         in  (pc', mRead (unPC pc') mem)

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
    addrs            = [a | (a, _, _) <- wp ^. lrucPsq . to OrdPSQ.toList, a >= sa && a <= ea]

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
  in  all verifyRegionSize (msys ^. regions . to IM.elems)

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
      (oldval, psq')                = OrdPSQ.alter queueAddr addr psq
      queueAddr Nothing             = (Nothing, Just (nextTick, word))
      queueAddr (Just (_, oldword)) = (Just oldword, Just (nextTick, word))
      updateNWrites (Just _) = 0
      updateNWrites Nothing  = 1
      doFlush mreg
        | mreg ^. nWritesPending >= maxPending
        = flushPending maxFlush sa mr'
        | otherwise
        = mreg
      mr'                            = mr & writesPending  .~ increaseTick (nextTick + 1) psq' maxSize
                                          & nWritesPending +~ updateNWrites oldval
      contentVal                     = (mr ^. contents) ! fromIntegral (addr - sa)
  in  (fromMaybe contentVal oldval, doFlush mr')

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
