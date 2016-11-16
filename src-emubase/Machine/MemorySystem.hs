{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-| Representation of system memory.

Emulated system memory represents memory as a collection of regions stored in an 'IntervalMap'. The 'IntervalMap' stores the
start and end addresses for the region; each region stores its contents and a read-only flag. Memory regions __/may not/__
overlap; they __/must/__ be distinct.

How to use: Add regions to an 'initialMemorySystem' via 'mkRAMRegion' or 'mkROMRegion'. Example snippet from the TRS-80
code:

> -- | Create the system's RAM
> installMem :: TRS80ModelISystem
>            -> Int
>            -> Vector Z80word
>            -> TRS80ModelISystem
> installMem sys memSize newROM =
>   sys & memory %~ mkROMRegion 0 newROM & memory %~ mkRAMRegion ramStart (memSize * 1024)

TODO: Allow update to a region that turns off the read-only flag (i.e., convert ROM to RAM.)
 -}

module Machine.MemorySystem
  ( MemorySystem(..)
  , MemoryRegion(..)
  , readOnly
  , contents
  , mkRAMRegion
  , mkROMRegion
  , mFetch
  , mFetchN
  , mFetchAndIncPC
  , mIncPCAndFetch
  , initialMemorySystem
  ) where

import Control.Lens (Lens', (|>), (^.), (%~), (&))
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as DVU
import qualified Data.IntervalMap.Interval as I
import qualified Data.IntervalMap.Generic.Strict as IM

import Machine.ProgramCounter
import Machine.Utils

-- | A memory region
data MemoryRegion addrType wordType where
  MR ::
    { _readOnly    :: Bool
    , _contents    :: Vector wordType
    -- ^ Memory region's contents (unboxed vector)
    } -> MemoryRegion addrType wordType
  deriving (Show)

-- | Lens for a memory region's read-only flag.
readOnly :: Lens' (MemoryRegion addrType wordType) Bool
readOnly f mregion = (\ro -> mregion { _readOnly = ro }) <$> f (_readOnly mregion)

-- | Lens for a memory region's contents
contents :: Lens' (MemoryRegion addrType wordType) (Vector wordType)
contents f mregion = (\content' -> mregion { _contents = content' }) <$> f (_contents mregion)

type MemRegionMap addrType wordType = IM.IntervalMap (I.Interval addrType) (MemoryRegion addrType wordType)

-- | A memory system, for a given address type and word type.
data MemorySystem addrType wordType where
  MSys :: (Ord addrType, Integral addrType, Num addrType, DVU.Unbox wordType) =>
    { _regions :: MemRegionMap addrType wordType
    } -> MemorySystem addrType wordType

regions :: Lens' (MemorySystem addrType wordType) (MemRegionMap addrType wordType)
regions f msys = (\regions' -> msys { _regions = regions' }) <$> f (_regions msys)

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
                                             MR { _readOnly = False
                                                , _contents = DVU.replicate len 0
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
                                           MR { _readOnly = True
                                              , _contents = romImg
                                              }
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

-- | Fetch a word from a memory address. Note: If the address does not correspond to a region (i.e., the address does not
-- intersect a region), zero is returned. (FIXME??)
mFetch :: (Integral addrType, Num wordType, DVU.Unbox wordType) =>
          MemorySystem addrType wordType
       -> addrType
       -> wordType
mFetch msys addr =
  let regs = IM.containing (msys ^. regions) addr
      getContent acc iv mr = acc |> ((mr ^. contents) ! fromIntegral (addr - I.lowerBound iv))
      vals = IM.foldlWithKey getContent [] regs
  in  if not (null vals)
      then head vals
         -- FIXME: Should something different happen here when data is requested from
         -- an unknown/unmapped region?
      else 0

-- | Fetch a sequence of words from memory. The start and end addresses do not have reside in the same memory region;
-- gaps between regions will be filled with zeroes.
mFetchN :: (Integral addrType, Num wordType, ShowHex addrType, DVU.Unbox wordType) =>
           MemorySystem addrType wordType
        -> addrType
        -> Int
        -> Vector wordType
mFetchN msys sa nWords =
  let regs          = IM.intersecting (msys ^. regions) (I.ClosedInterval sa (sa + fromIntegral nWords))
      go (addr, remaining, vl) ivl reg
        | addr > lb  = let nb     = min (ub - addr) (fromIntegral remaining)
                           vl'    = ((vl [DVU.slice (fromIntegral (addr - lb)) (fromIntegral nb) cts]) ++)
                           accum' = (ub + 1, remaining - (fromIntegral nb), vl')
                       in  (accum', reg)
        | addr == lb = let nb = min (ub - addr) (fromIntegral remaining)
                           accum' = (ub - nb + 1, remaining - (fromIntegral nb), ((vl [DVU.slice 0 (fromIntegral nb) cts]) ++))
                       in  (accum', reg)
        | addr < lb  = let nb     = min (ub - addr) (fromIntegral remaining)
                           vl'    = ((vl [DVU.replicate (fromIntegral (lb - addr)) 0, DVU.slice 0 (fromIntegral nb) cts]) ++)
                           accum' = (ub - nb + 1, remaining - (fromIntegral nb), vl')
                       in  (accum', reg)
        -- Squelch GHC pattern warning...
        | otherwise   = error ("How'd I get here? addr = " ++ as0xHexS addr ++ " remaining " ++ show remaining)
        where
          lb  = I.lowerBound ivl
          ub  = I.upperBound ivl
          cts = reg ^. contents
      ((_, remain', accum), _)   = IM.mapAccumWithKey go (sa, nWords, ([] ++)) regs
      endfill                    = if remain' == 0
                                   then []
                                   else [DVU.replicate remain' 0]
  in  (DVU.concat (accum endfill))

-- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
-- pair.
mFetchAndIncPC :: (PCOperation addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -- ^ The program counter
               -> MemorySystem addrType wordType
               -- ^ The memory system
               -> (ProgramCounter addrType, wordType)
mFetchAndIncPC pc mem = (pc + 1, withPC pc (mFetch mem))

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndFetch :: (PCOperation addrType, Num wordType, DVU.Unbox wordType) =>
                  ProgramCounter addrType
               -> MemorySystem addrType wordType
               -> (ProgramCounter addrType, wordType)
mIncPCAndFetch pc mem = let pc' = pc + 1
                        in  (pc', withPC pc' (mFetch mem))

initialMemorySystem :: (Integral addrType, DVU.Unbox wordType) => MemorySystem addrType wordType
initialMemorySystem = MSys { _regions = IM.empty }
