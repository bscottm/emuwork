{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Machine.ProgramCounter where

import Data.Data
import           Data.Bits
import qualified Data.Vector.Unboxed           as DVU
import           Machine.Utils
import           Machine.MemorySystem

-- | Generic program counter
newtype ProgramCounter addrType = PC { unPC :: addrType }
  deriving (Functor, Num, Eq, Bits, FiniteBits, Enum, Ord, Integral, Real, Data, Typeable)

-- | Since 'ProgramCounter' is a 'Functor', also make it 'Applicative'
instance Applicative ProgramCounter where
  pure = PC
  (PC f) <*> (PC pc) = PC (f pc)

-- | Make program counters show-able as something coherent.
instance (ShowHex addrType) => Show (ProgramCounter addrType) where
  show pc = "PC(" ++ as0xHexS pc ++ ")"

instance (ShowHex addrType) => ShowHex (ProgramCounter addrType) where
  as0xHex = as0xHex . unPC
  asHex   = asHex . unPC

-- | Displace a program counter by a signed amount, where the displacement may not be the same size
-- (in bits) as the program counter.
pcDisplace :: (SignExtend dispType, Num addrType, FiniteBits addrType) =>
              dispType
           -> ProgramCounter addrType
           -> ProgramCounter addrType
pcDisplace disp (PC pc) = PC (pc + signExtend disp)

mReadAndIncPC :: ( Integral addrType
                 , Integral wordType
                 , DVU.Unbox wordType
                 )
              => ProgramCounter addrType
              -> MemorySystem addrType wordType
              -> (ProgramCounter addrType, (wordType, MemorySystem addrType wordType))
mReadAndIncPC pc msys = (pc + 1, mRead (unPC pc) msys)

-- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
mIncPCAndRead :: ( Integral addrType
                 , Integral wordType
                 , DVU.Unbox wordType
                 )
              => ProgramCounter addrType
              -> MemorySystem addrType wordType
              -> (ProgramCounter addrType, (wordType, MemorySystem addrType wordType))
mIncPCAndRead pc sys = let pc' = pc + 1
                       in  (pc', mRead (unPC pc') sys)
