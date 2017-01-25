{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machine.ProgramCounter where

import           Data.Bits
import           Machine.Utils

-- | Generic program counter
newtype ProgramCounter addrType = PC { unPC :: addrType }
  deriving (Functor, Num, Eq, Bits, FiniteBits, Enum, Ord, Integral, Real)

-- | Since 'ProgramCounter' is a 'Functor', also make it 'Applicative'
instance Applicative ProgramCounter where
  pure = PC
  (PC f) <*> (PC pc) = PC (f pc)

-- | Make program counters show-able as something coherent.
instance (ShowHex addrType) => Show (ProgramCounter addrType) where
  show (PC pc) = "PC " ++ as0xHexS pc

-- | Displace a program counter by a signed amount, where the displacement may not be the same size
-- (in bits) as the program counter.
pcDisplace :: (SignExtend dispType, Num addrType, FiniteBits addrType) =>
              dispType
           -> ProgramCounter addrType
           -> ProgramCounter addrType
pcDisplace disp (PC pc) = PC (pc + signExtend disp)

instance (ShowHex addrType) => ShowHex (ProgramCounter addrType) where
  as0xHex pc = as0xHex (unPC pc)
  asHex pc   = asHex (unPC pc)
