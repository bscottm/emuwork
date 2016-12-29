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
  ( -- * Memory Regions
    mkRAMRegion
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

import           Control.Arrow                 (first, (***))
import           Control.Lens                  (over, to, view, set, (%~), (&), (+~), (-~), (.~), (^.), (|>))
import           Control.Monad.State.Strict    (runState, state)
import qualified Data.Foldable                 as Fold (foldl')
import qualified Data.HashMap.Strict           as H
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.IntervalMap.Interval     as I
import           Data.Maybe                    (fromMaybe)
import qualified Data.OrdPSQ                   as OrdPSQ
import           Data.Traversable              (traverse)
import           Data.Vector.Unboxed           (Vector, (!), (//))
import qualified Data.Vector.Unboxed           as DVU
import           Prelude                       hiding (lookup)

import           Machine.Device
import           Machine.ProgramCounter
import           Machine.Types
import           Machine.Utils

-- import           Debug.Trace

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
            -> EmulatedSystem procType insnSet addrType wordType
            -> EmulatedSystem procType insnSet addrType wordType
mkRAMRegion sa len sys =
  let msys      = sys ^. memory
      ea        = sa + fromIntegral len
      newRegion = RAMRegion { _contents       = DVU.replicate len 0
                            , _writesPending  = initialLRU maxPending
                            , _nWritesPending = 0
                            }
  in    if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
        then set memory (over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys) sys
        else error ("mkRAMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Create a new read-only (ROM) memory region. __/Note:/__ memory regions may not overlap; 'error' will signal if this
-- precondition fails.
mkROMRegion :: (Ord addrType, Num addrType, ShowHex addrType, DVU.Unbox wordType) =>
               addrType
            -- ^ Region's start address
            -> Vector wordType
            -- ^ ROM image to insert as the region's contents
            -> EmulatedSystem procType insnSet addrType wordType
            -- ^ Memory system to alter
            -> EmulatedSystem procType insnSet addrType wordType
            -- ^ Resulting memory system with the inserted ROM region
mkROMRegion sa romImg sys =
  let msys      = sys ^. memory
      ea        = sa + fromIntegral (DVU.length romImg)
      newRegion = ROMRegion { _contents = romImg }
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then set memory (over regions (IM.insertWith const (I.IntervalCO sa ea) newRegion) msys) sys
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Create a new region for a memory-mapped device.
mkDevRegion :: ( Ord addrType
               , Num addrType
               , ShowHex addrType
               , DVU.Unbox wordType
               ) =>
               addrType
            -- ^ Start address of the region
            -> addrType
            -- ^ End address of the region
            -> Device addrType wordType
            -- ^ The device type
            -> EmulatedSystem procType insnSet addrType wordType
            -- ^ Memory region to augment
            -> (Int, EmulatedSystem procType insnSet addrType wordType)
            -- ^ Resulting memory system with the device region inserted
mkDevRegion sa ea dev sys =
  let msys   = sys ^. memory
      devIdx = sys ^. nDevices
  in  if   IM.null (IM.intersecting (msys ^. regions) (I.ClosedInterval sa ea))
      then (devIdx, sys & nDevices    +~ 1
                        & memory      .~ (msys & regions %~ IM.insertWith const (I.IntervalCO sa ea) (DevMemRegion devIdx))
                        & deviceTable %~ H.insert devIdx dev)
      else error ("mkDevRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Read/write memory
-- ~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Return type for 'mRead'. It has to be this way because devices __can__ (and usually __do__) change
-- state when read, producing an altered 'EmulatedSystem'
type MemRead procType insnSet addrType wordType = (wordType, EmulatedSystem procType insnSet addrType wordType)
-- | Return type for 'mReadN'. Devices __can__ and usually __do__ change state when read, producing an altered
-- 'MemorySystem'.
type MemReadN procType insnSet addrType wordType = (Vector wordType, EmulatedSystem procType insnSet addrType wordType)

-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned.
mRead :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
         addrType
      -- ^ Address to read
      -> EmulatedSystem procType insnSet addrType wordType
      -- ^ Current system state
      -> MemRead procType insnSet addrType wordType
      -- ^ Value read and updated system state
mRead !addr !sys =
  let -- Cute use of arrows to operate on the pair returned by 'devRead':
      getContent (vals', sys') mr (DevMemRegion devIdx) =
        let doMemRead dev' = (vals' |>) *** doDevUpd $ deviceRead (addr - baseOffset) dev'
            doDevUpd  dev' = sys' & deviceTable %~ H.update (\_ -> Just dev') devIdx
            defaultVal     = (vals' |> 0, sys')
            baseOffset     = I.lowerBound mr
        in  maybe defaultVal doMemRead (H.lookup devIdx (sys' ^. deviceTable))
      getContent (vals', sys') iv mr =
        let memval = fromMaybe (view contents mr ! fromIntegral (addr - I.lowerBound iv))
                               (lookupPendingWrite addr (mr ^. writesPending))
        in (vals' |> memval, sys')
      -- FIXME: Need another function that throws an exception (polluting everything with the IO monad) or otherwise signals
      -- that the read was from unmapped memory. Or: Provide a function that checks the address is mapped, let that throw
      -- an exception, otherwise return the value.
      --
      -- Here, just return 0 if no value could be read from the memory regions.
      ensureValue []   = 0
      ensureValue vals = head vals
      -- And another use of an arrow on a pair.
  in  first ensureValue $ IM.foldlWithKey getContent ([], sys) (IM.containing (sys ^. memory . regions) addr)

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mReadN :: (Integral addrType,
           Num wordType,
           ShowHex addrType,
           DVU.Unbox wordType) =>
          addrType
       -- ^ Starting address
       -> Int
       -- ^ Number of words to read
       -> EmulatedSystem procType insnSet addrType wordType
       -- ^ The memory system from which to read
       -> MemReadN procType insnSet addrType wordType
       -- ^ Contents read from memory and updated system state
mReadN !sa !nWords !sys
  {- Trivial optimization: Could also do this for other small numbers, e.g., nWords <= 4 -}
  | nWords == 1
  = first DVU.singleton (mRead sa sys)
  | otherwise
  = let memRegions                           = IM.intersecting (sys ^. memory . regions) (I.ClosedInterval sa ea)
        ea                                   = sa + fromIntegral (nWords - 1)
        ((_, remain', accum, resultSys), _)  = IM.mapAccumWithKey accumContent (sa, nWords, ([] ++), sys) memRegions

        accumContent (addr, remaining, vl, sys') ivl reg
          {-  | trace ("addr = " ++ as0xHexS addr ++ " remaining = " ++ show remaining) False = undefined -}
          | addr >= lb  = let (memVals, sys'')  = getContent (fromIntegral (addr - lb)) nb sys' reg
                              vl'               = (vl [memVals] ++)
                              nb                = min (fromIntegral (ub - addr)) remaining
                              accum'            = (addr + fromIntegral nb, remaining - nb, vl', sys'')
                          in  (accum', reg)
          | addr < lb   = let flen              = fromIntegral (lb - addr)
                              nb'               = min (fromIntegral (ub - lb)) (remaining - flen)
                              nread             = nb' + flen
                              (memVals, sys'')  = getContent 0 nb' sys' reg
                              vl'               = (vl [DVU.replicate flen 0, memVals] ++)
                              accum'            = (addr + fromIntegral nread, remaining - nread, vl', sys'')
                          in  (accum', reg)
          -- Squelch GHC pattern warning... because it really can't happen.
          | otherwise   = error ("How'd I get here? addr = " ++ as0xHexS addr ++ " remaining " ++ show remaining)
          where
            lb       = I.lowerBound ivl
            ub       = I.upperBound ivl

        getContent offs len sys' (DevMemRegion devIdx) =
          let dev              = fromMaybe (error ("MemmorySystem.mReadN: Device not found for device index " ++ show devIdx))
                                           (H.lookup devIdx (sys ^. deviceTable))
              -- Ensure the memory system has an updated device after reading... the sensible thing
              updDevTable dev' = sys' & deviceTable %~ H.update (\_ -> Just dev') devIdx
              -- Generate the addresses to read from the device, turn them into StateT device reader functiosn, then
              -- runState over the resulting list. (Note: Eta reduction here, 'dev' is the implied parameter.)
              readDevice       = runState (traverse (state . deviceRead) [fromIntegral a | a <- [offs..offs + len - 1]])
          in  DVU.fromList *** updDevTable $ readDevice dev

        getContent offs len sys' memRegion             = (DVU.slice offs len (memRegion ^. contents), sys')

        endfill                    = [DVU.replicate remain' 0]
        -- And ensure that pending writes in the regions are also included. This uses the same pattern that 'showS' uses,
        -- threading a list function across the regions so that concatenation only happens once and has linear performance.
        writes                = Fold.foldl' getWrites ([] ++) memRegions []
        getWrites pend reg    = (pend (reg ^. writesPending . lrucPsq . to filterWrites) ++)
        filterWrites psq      = [(a - sa, v) | (a, _, v) <- OrdPSQ.toList psq, a >= sa && a <= ea ]
        idxvec                = DVU.fromList (map (fromIntegral . fst) writes)
        valvec                = DVU.fromList (map snd writes)
    in  (DVU.update_ ((DVU.concat . accum) endfill) idxvec valvec, resultSys)

-- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
-- pair.
mReadAndIncPC :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -- ^ Current program counter
               -> EmulatedSystem procType insnSet addrType wordType
               -- ^ The memory system from which the word will be fetched
               -> (ProgramCounter addrType, MemRead procType insnSet addrType wordType)
               -- ^ Updated program counter and fetched word
mReadAndIncPC !pc !sys = (pc + 1, mRead (unPC pc) sys)

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndRead :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
              -- ^ Current progracm counter
               -> EmulatedSystem procType insnSet addrType wordType
               -- ^ Memory system from which word will be fetched
               -> (ProgramCounter addrType, MemRead procType insnSet addrType wordType)
               -- ^ Updated program counter and fetched word
mIncPCAndRead !pc !sys = let pc' = pc + 1
                         in  (pc', mRead (unPC pc') sys)

-- | Write a word into the memory system: inserts the word into the pending write LRU cache, flushing the cache to make
-- space if necessary, commiting updates to the underlying region's contents. If the memory region is not 'readWrite',
-- then nothing happens.
mWrite :: (Integral addrType, DVU.Unbox wordType) =>
          addrType
       -- ^ Address to write to
       -> wordType
       -- ^ Value to write
       -> EmulatedSystem procType insnSet addrType wordType
       -- ^ Current system state
       -> EmulatedSystem procType insnSet addrType wordType
       -- ^ Updated system state
mWrite !addr !word !sys =
  let regs                                  = IM.intersecting (sys ^. memory . regions) (I.ClosedInterval addr addr)
      writeRegion sys' iv (DevMemRegion didx) =
        let doMemWrite dev' = doDevUpd $ deviceWrite (addr - baseOffset) word dev'
            doDevUpd  dev'  = sys' & deviceTable %~ H.update (\_ -> Just dev') didx
            baseOffset      = I.lowerBound iv
        in  maybe (error ("MemorySystem.mWrite: Device index " ++ show didx ++ " not found in device table."))
                  doMemWrite
                  (H.lookup didx (sys' ^. deviceTable))

      writeRegion sys' iv _mr              = set memory (over regions (IM.update (queuePending (I.lowerBound iv)) iv) msys) sys'
        where msys = sys ^. memory
      -- Note: currently discarding the old value, (oldval, mr')
      queuePending lb mr@RAMRegion{}        = Just (snd (queueLRU addr word lb mr))
      -- Otherwise, don't write anything.
      queuePending _lb mr                   = Just mr
  in  IM.foldlWithKey writeRegion sys regs

-- | Write a block of memory, respecting the memory system's type
mWriteN :: (Integral addrType, DVU.Unbox wordType) =>
           addrType
        -- ^ Starting address
        -> Vector wordType
        -- ^ Content to write
        -> EmulatedSystem procType insnSet addrType wordType
        -- ^ Current system state
        -> EmulatedSystem procType insnSet addrType wordType
        -- ^ Updated system state
mWriteN = doWrite True

-- | Patch (forcibly overwrite) memory. This does not obey or check the 'readWrite' flag and will truncate the memory patch
-- if it extends beyond a region's upper bound.
mPatch :: (Integral addrType, DVU.Unbox wordType) =>
          addrType
       -- ^ Starting address where patch is applied
       -> Vector wordType
       -- ^ The patch
       -> EmulatedSystem procType insnSet addrType wordType
       -- ^ Memory system to which patch will be applied
       -> EmulatedSystem procType insnSet addrType wordType
       -- ^ Patched memory system
mPatch = doWrite False

-- | Write to memory: 'mPatch' and 'mWriteN' share this code.
doWrite :: (Integral addrType, DVU.Unbox wordType) =>
           Bool
        -- ^ Preserve read-only regions: If True, prevents writing to 'ROMRegion's
        -> addrType
        -- ^ Starting address where patch is applied
        -> Vector wordType
        -- ^ The patch
        -> EmulatedSystem procType insnSet addrType wordType
        -- ^ Memory system to which patch will be applied
        -> EmulatedSystem procType insnSet addrType wordType
        -- ^ Patched memory system
doWrite !preserveRO !paddr !patch !sys =
  let minterval                = I.ClosedInterval paddr ea
      ea                       = paddr + fromIntegral (DVU.length patch - 1)
      ((_, updatedSys), mregs) = IM.mapAccumWithKey updContent (paddr, sys) (sys ^. memory . regions)
      updContent (sa, sys') iv reg
        -- Don't molest ROM regions if preserving read-only
        | ROMRegion{} <- reg
        , preserveRO
        = ((ub + 1, sys'), reg)
        -- Devices...
        | (DevMemRegion devIdx) <- reg
        , iv `I.overlaps` minterval
        = let dev              = fromMaybe (error ("MemmorySystem.mWriteN: Device not found for device index " ++ show devIdx))
                                           (H.lookup devIdx (sys' ^. deviceTable))
              -- Ensure the memory system has an updated device after reading... the sensible thing
              updDevTable dev' = sys' & deviceTable %~ H.update (\_ -> Just dev') devIdx
          in  ((ub + 1, updDevTable $ bulkDeviceWrite (sa' - lb) pslice dev), reg)
        -- RAMRegion and non-preserving ROMRegion writes
        | iv `I.overlaps` minterval
        = ((ub + 1, sys'), reg & contents       %~ (\vec -> DVU.update_ vec pidxs pslice)
                               & writesPending  .~ culledPending
                               & nWritesPending -~ nCulled)
        | otherwise
        = ((ub + 1, sys'), reg)
        where
          lb                       = I.lowerBound iv
          ub                       = I.upperBound iv
          sa'                      = max sa lb
          sidx                     = fromIntegral (sa' - paddr)
          plen                     = min (DVU.length patch - sidx) (fromIntegral (ub - sa'))
          idxoffs                  = fromIntegral (sa' - lb)
          pslice                   = DVU.slice sidx plen patch
          pidxs                    = DVU.generate plen (+ idxoffs)
          (nCulled, culledPending) = cullPendingWrites sa ea (reg ^. writesPending)
  in  updatedSys & memory . regions .~ mregs

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
