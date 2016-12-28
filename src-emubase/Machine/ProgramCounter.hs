{-# LANGUAGE GADTs #-}

module Machine.ProgramCounter where

import           Control.Arrow
import           Data.Bits

import           Machine.Utils

-- | Generic program counter
newtype ProgramCounter addrType = PC { unPC :: addrType }

-- | Make 'ProgramCounter' a 'Functor'
instance Functor ProgramCounter where
  fmap f (PC pc) = PC (f pc)

-- | Since 'ProgramCounter' is a 'Functor', also make it 'Applicative'
instance Applicative ProgramCounter where
  pure = PC
  (PC f) <*> (PC pc) = PC (f pc)

-- | Make program counters behave like numeric types
instance (Num addrType) => Num (ProgramCounter addrType) where
  (PC a) + (PC b) = PC (a + b)
  (PC a) - (PC b) = PC (a - b)
  (PC a) * (PC b) = PC (a * b)
  abs (PC a)      = PC (abs a)
  signum (PC a)   = PC (signum a)
  fromInteger a   = PC (fromInteger a)

-- | Make program counters comparable
instance (Ord addrType) => Ord (ProgramCounter addrType) where
  compare (PC a) (PC b) = compare a b

-- | Provide equality comparisons for program counters
instance Eq addrType => Eq (ProgramCounter addrType) where
  (PC a) == (PC b) = a == b

-- | Make program counters show-able as something coherent.
instance (ShowHex addrType) => Show (ProgramCounter addrType) where
  show (PC pc) = "PC " ++ as0xHexS pc

-- | Admit 'ProgramCounter' into the Integral class
instance (Integral addrType) => Integral (ProgramCounter addrType) where
  quotRem (PC a) (PC b) = (PC *** PC) $ quotRem a b
  toInteger (PC a) = toInteger a

-- | Extra hair for Integral
instance (Real addrType) => Real (ProgramCounter addrType) where
  toRational (PC a) = toRational a

-- | Extra hair for Integral
instance (Enum addrType) => Enum (ProgramCounter addrType) where
  toEnum = PC . toEnum
  fromEnum (PC a) = fromEnum a

-- | Admit 'ProgramCounter' into the 'Bits' type class
instance (Bits addrType, FiniteBits addrType) => Bits (ProgramCounter addrType) where
  {-# INLINE bit #-}
  {-# INLINE testBit #-}
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE shift #-}
  {-# INLINE shiftR #-}
  {-# INLINE shiftL #-}
  {-# INLINE bitSizeMaybe #-}
  {-# INLINE bitSize #-}
  {-# INLINE popCount #-}
  {-# INLINE isSigned #-}
  bit b                         = PC (bit b)
  testBit (PC a)                = testBit a
  (PC a) .&.   (PC b)           = PC (a .&. b)
  (PC a) .|.   (PC b)           = PC (a .|. b)
  (PC a) `xor` (PC b)           = PC (a `xor` b)
  complement (PC a)             = PC (complement a)
  (PC a) `shift` amt            = PC (a `shift` amt)
  (PC a) `shiftL` amt           = PC (a `shiftL` amt)
  (PC a) `shiftR` amt           = PC (a `shiftR` amt)
  (PC a) `rotate` amt           = PC (a `rotate` amt)
  bitSizeMaybe (PC a)           = bitSizeMaybe a
  bitSize (PC a)                = finiteBitSize a
  popCount (PC a)               = popCount a
  isSigned (PC a)               = isSigned a

-- | Admit 'ProgramCounter' into the 'FiniteBits' type class
instance (FiniteBits addrType) => FiniteBits (ProgramCounter addrType) where
  finiteBitSize (PC addr) = finiteBitSize addr

-- | Displace a program counter by a signed amount, where the displacement may not be the same size
-- (in bits) as the program counter.
pcDisplace :: (SignExtend dispType, Num addrType, FiniteBits addrType) =>
              dispType
           -> ProgramCounter addrType
           -> ProgramCounter addrType
pcDisplace disp (PC pc) = PC (pc + signExtend disp)
