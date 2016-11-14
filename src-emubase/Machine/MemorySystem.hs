{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Machine.MemorySystem where

import Control.Lens
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as DVU
import qualified Data.IntervalMap.Interval as I
import qualified Data.IntervalMap.Generic.Strict as IM

-- import Debug.Trace

import Machine.ProgramCounter
import Machine.Utils

-- | Memory region
data MemoryRegion addrType wordType where
  MR ::
    { _readOnly    :: Bool
    , _startAddr   :: addrType
    , _endAddr     :: addrType
    , _contents    :: Vector wordType
    } -> MemoryRegion addrType wordType
  deriving (Show)

readOnly :: Lens' (MemoryRegion addrType wordType) Bool
readOnly f mregion = (\ro -> mregion { _readOnly = ro }) <$> f (_readOnly mregion)

startAddr :: Lens' (MemoryRegion addrType wordType) addrType
startAddr f mregion = (\sa -> mregion { _startAddr = sa }) <$> f (_startAddr mregion)

endAddr :: Lens' (MemoryRegion addrType wordType) addrType
endAddr f mregion = (\ea -> mregion { _endAddr = ea }) <$> f (_endAddr mregion)

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
                                                , _startAddr = sa
                                                , _endAddr   = ea
                                                , _contents = DVU.replicate len 0
                                                }
        else error ("mkRAMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

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
                                              , _startAddr = sa
                                              , _endAddr = ea
                                              , _contents = romImg
                                              }
      else error ("mkROMRegion: " ++ as0xHexS sa ++ "-" ++ as0xHexS ea ++ " overlaps with existing regions.")

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

mFetchN :: (Integral addrType, DVU.Unbox wordType) =>
           MemorySystem addrType wordType
        -> addrType
        -> Int
        -> Vector wordType
mFetchN msys sa nWords =
  let regs = IM.intersecting (msys ^. regions) (I.ClosedInterval sa (sa + fromIntegral nWords))
  in  if IM.size regs == 1
      then let reg = head (IM.elems regs)
               regSA = reg ^. startAddr
               regEA = reg ^. endAddr
           in  DVU.slice (fromIntegral (sa - regSA)) (min (fromIntegral (regEA - sa)) nWords) (reg ^. contents)
      else DVU.empty

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
