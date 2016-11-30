module Machine.ProgramCounter where

import           Control.Arrow
import           Control.Comonad
import           Data.Bits
import           Data.Word

import           Machine.Utils

-- | Generic program counter
data ProgramCounter addrType where
  PC :: !addrType
     -> ProgramCounter addrType

-- | Make 'ProgramCounter' a 'Functor'
instance Functor ProgramCounter where
  fmap f (PC pc) = PC (f pc)

-- | Since 'ProgramCounter' is a 'Functor', also make it 'Applicative'
instance Applicative ProgramCounter where
  pure = PC
  (PC f) <*> (PC pc) = PC (f pc)

-- | 'extend' and 'extract' 'ProgramCounter' values
instance Comonad ProgramCounter where
  extract (PC pc) = pc
  extend  f pc    = PC (f pc)

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

-- | Admit RelativePC into the Integral class
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

-- | Relative program counter data; 'dispType' should be a signed type of the same size as 'ProgramCounter's 'addrType'
data RelativePC dispType where
  RelativePC :: dispType
             -> RelativePC dispType

-- | Admit 'RelativePC' to the 'Num' type class
instance (Integral dispType) => Num (RelativePC dispType) where
  (RelativePC a) + (RelativePC b) = RelativePC (a + b)
  (RelativePC a) - (RelativePC b) = RelativePC (a - b)
  (RelativePC a) * (RelativePC b) = RelativePC (a * b)
  abs (RelativePC a)              = RelativePC . abs $ a
  signum (RelativePC a)           = RelativePC . signum $ a
  fromInteger                     = RelativePC . fromInteger

-- | Allow comparisons between 'RelativePC' program counter displacements
instance Ord dispType => Ord (RelativePC dispType) where
  compare (RelativePC a) (RelativePC b) = compare a b

-- | Equality comparisons
instance Eq dispType => Eq (RelativePC dispType) where
  (RelativePC a) == (RelativePC b) = a == b

-- | Admit 'RelativePC' into the 'Integral' class
instance (Integral dispType) => Integral (RelativePC dispType) where
  quotRem (RelativePC a) (RelativePC b) = (RelativePC *** RelativePC) $ quotRem a b
  toInteger (RelativePC a) = toInteger a

-- | Extra hair for Integral
instance (Real dispType, Integral dispType) => Real (RelativePC dispType) where
  toRational (RelativePC a) = toRational a

-- | Extra hair for Integral
instance (Enum dispType) => Enum (RelativePC dispType) where
  toEnum = RelativePC . toEnum
  fromEnum (RelativePC a) = fromEnum a

-- | Admit 'RelativePC' into the 'Bits' type class
instance (Bits dispType, FiniteBits dispType) => Bits (RelativePC dispType) where
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
  bit b                                 = RelativePC (bit b)
  testBit (RelativePC a)                = testBit a
  (RelativePC a) .&.   (RelativePC b)   = RelativePC (a .&. b)
  (RelativePC a) .|.   (RelativePC b)   = RelativePC (a .|. b)
  (RelativePC a) `xor` (RelativePC b)   = RelativePC (a `xor` b)
  complement (RelativePC a)             = RelativePC (complement a)
  (RelativePC a) `shift` amt            = RelativePC (a `shift` amt)
  (RelativePC a) `shiftL` amt           = RelativePC (a `shiftL` amt)
  (RelativePC a) `shiftR` amt           = RelativePC (a `shiftR` amt)
  (RelativePC a) `rotate` amt           = RelativePC (a `rotate` amt)
  bitSizeMaybe (RelativePC a)           = bitSizeMaybe a
  bitSize (RelativePC a)                = finiteBitSize a
  popCount (RelativePC a)               = popCount a
  isSigned (RelativePC a)               = isSigned a

-- | Admit 'RelativePC' into the 'FiniteBits' type class
instance (FiniteBits dispType) => FiniteBits (RelativePC dispType) where
  finiteBitSize (RelativePC disp) = finiteBitSize disp

-- | Basic program counter type class: increment, decrement, and displace
class (Num pcType, Integral pcType, FiniteBits pcType) => PCOperation pcType where
  -- | Displace the program counter by a displacement amount (positive or negative).
  pcDisplace :: (SignExtend dispType) => RelativePC dispType
             -> pcType
             -> pcType
  pcDisplace disp pc = fromIntegral pc + fromIntegral disp

-- | Do an action on a program counter
withPC :: ProgramCounter addrType -> (addrType -> value) -> value
withPC (PC pc) f = f pc
{-# INLINE withPC #-}

-- | Instantiate 'PCOperation' operations for 'ProgramCounter'
instance (Num addrType, Integral addrType, FiniteBits addrType) => PCOperation (ProgramCounter addrType) where
  pcDisplace (RelativePC disp) (PC pc) = PC (pc + signExtend disp)

instance PCOperation Word8
instance PCOperation Word16
instance PCOperation Word32
instance PCOperation Word64
