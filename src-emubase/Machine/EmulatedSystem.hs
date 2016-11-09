{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}

-- | General data structures and type classes for emulated processors.
module Machine.EmulatedSystem where

import Data.Bits
import Data.Data
import Control.Lens
import Data.Vector.Unboxed (Vector)
import qualified Data.Text as T
import Control.Comonad

import Machine.Utils

-- | Generic program counter
data ProgramCounter addrType where
  PC :: addrType
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
  quotRem (PC a) (PC b) = let res = quotRem a b
                          in  ((PC . fst) res, (PC . snd) res)
  toInteger (PC a) = toInteger a

-- | Extra hair for Integral
instance (Real addrType) => Real (ProgramCounter addrType) where
  toRational (PC a) = toRational a

-- | Extra hair for Integral
instance (Enum addrType) => Enum (ProgramCounter addrType) where
  toEnum a = (PC . toEnum) a
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
  testBit (PC a) b              = testBit a b
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
  fromInteger a                   = RelativePC . fromInteger $ a

-- | Allow comparisons between 'RelativePC' program counter displacements
instance Ord dispType => Ord (RelativePC dispType) where
  compare (RelativePC a) (RelativePC b) = compare a b

-- | Equality comparisons
instance Eq dispType => Eq (RelativePC dispType) where
  (RelativePC a) == (RelativePC b) = a == b

-- | Admit 'RelativePC' into the 'Integral' class
instance (Integral dispType) => Integral (RelativePC dispType) where
  quotRem (RelativePC a) (RelativePC b) = let res = quotRem a b
                                          in  ((RelativePC . fst) res, (RelativePC . snd) res)
  toInteger (RelativePC a) = toInteger a

-- | Extra hair for Integral
instance (Real dispType, Integral dispType) => Real (RelativePC dispType) where
  toRational (RelativePC a) = toRational a

-- | Extra hair for Integral
instance (Enum dispType) => Enum (RelativePC dispType) where
  toEnum a = (RelativePC . toEnum) a
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
  testBit (RelativePC a) b              = testBit a b
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
  -- | Increment the program counter
  pcInc      :: pcType
             -> pcType
  pcInc pc = pc + 1
  -- | Decrement the program counter
  pcDec      :: pcType
             -> pcType
  pcDec pc = pc - 1
  -- | Displace the program counter by a displacement amount (positive or negative).
  pcDisplace :: (Integral dispType, FiniteBits dispType, SignExtend dispType) =>
                RelativePC dispType
             -> pcType
             -> pcType
  pcDisplace disp pc = fromIntegral pc + fromIntegral disp

-- | Do an action on a program counter
withPC :: ProgramCounter addrType -> (addrType -> value) -> value
withPC (PC pc) f = f pc
{-# INLINE withPC #-}

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procType addrType instructionSet where
  EmulatedProcessor ::
    { _procPrettyName :: String                 -- ^ Pretty name for the emulated processor
    , _internals      :: procType               -- ^ Processor-specific internal data.
    } -> EmulatedProcessor  procType addrType instructionSet

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

-- | Type class for memory system operations.
class MemoryOps memSys addrType wordType where
  -- | Fetch a single entity from memory
  mFetch :: memSys
        -> addrType
        -> wordType

  -- | Fetch multiple entities from memory
  mFetchN :: memSys
         -> addrType
         -> Int
         -> Vector wordType

  -- | Fetch an entity from memory at the current program counter, return the (incremented pc, contents)
  -- pair.
  mFetchAndIncPC :: (PCOperation addrType) =>
                    ProgramCounter addrType
                    -- ^ The program counter
                 -> memSys
                 -- ^ The memory system
                 -> (ProgramCounter addrType, wordType)
  mFetchAndIncPC pc mem = (pcInc pc, withPC pc (mFetch mem))

  -- | Fetch an entity from memory, pre-incrementing the program counter, returning the (incremented pc, contents)
  mIncPCAndFetch :: (PCOperation addrType) =>
                    ProgramCounter addrType
                 -> memSys
                 -> (ProgramCounter addrType, wordType)
  mIncPCAndFetch pc mem = let pc' = pcInc pc
                          in  (pc', withPC pc' (mFetch mem))

-- | A memory system, for a given address type and word type.
data MemorySystem addrType wordType where
  MemorySystem :: (MemoryOps memSysType addrType wordType) =>
                  memSysType
               -> MemorySystem addrType wordType

-- | Generic representation of instruction decoder outputs
data DecodedInsn instructionSet addrType where
  -- A decoded instruction
  DecodedInsn :: ProgramCounter addrType
              -> instructionSet
              -> DecodedInsn instructionSet addrType
  -- An address fetched from memory, independent of endian-ness
  DecodedAddr :: ProgramCounter addrType
              -> addrType
              -> DecodedInsn instructionSet addrType

-- | Processor operations type class
class ProcessorOps insnSet addrType wordType where
  -- | Instruction decoder, for disassembly and execution
  idecode :: ProgramCounter addrType
          -- ^ Current program counter, from where instructions are fetched
          -> MemorySystem addrType wordType
          -- ^ The memory system
          -> DecodedInsn insnSet addrType
          -- ^ The decoded instruction

-- | 'EmulatedSystem' encapsulates the various parts required to emulate a system (processor, memory, ...)
data EmulatedSystem procInternals addrType wordType instructionSet where
  EmulatedSystem :: (ProcessorOps instructionSet addrType wordType) =>
    { _processor  :: EmulatedProcessor procInternals addrType instructionSet
                     -- ^ System processor
    , _memory     :: MemorySystem addrType wordType
                     -- ^ System memory
    , _sysName    :: String
                  -- ^ The system's name, e.g. "Null/dummy processor"
    , _sysAliases :: [String]
                  -- ^ System identity aliases, e.g., "null", "trs80-model-I" used to identify the
                  -- emulator.
    } -> EmulatedSystem procInternals addrType wordType instructionSet

-- Need to manually generate the lenses due to the constraint on EmulatedSystem

processor :: Lens' (EmulatedSystem procType addrType wordType insnSet) (EmulatedProcessor procType addrType insnSet)
processor f sys = (\proc -> sys { _processor = proc }) <$> f (_processor sys)

memory :: Lens' (EmulatedSystem procType addrType wordType insnSet) (MemorySystem addrType wordType)
memory f sys = (\msys -> sys { _memory = msys }) <$> f (_memory sys)

sysName :: Lens' (EmulatedSystem procType addrType wordType insnSet) String
sysName f sys = (\name -> sys { _sysName = name }) <$> f (_sysName sys)

sysAliases :: Lens' (EmulatedSystem procType addrType wordType insnSet) [String]
sysAliases f sys = (\aliases -> sys { _sysAliases = aliases }) <$> f (_sysAliases sys)

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | A (symbolic|absolute) address
data SymAbsAddr addrType where
  AbsAddr :: addrType
          -> SymAbsAddr addrType
  SymAddr :: T.Text
          -> SymAbsAddr addrType
  deriving (Typeable, Data)

instance (ShowHex addrType) => Show (SymAbsAddr addrType) where
  show (AbsAddr addr)  = as0xHexS addr
  show (SymAddr label) = show label

-- | Instantiate 'PCOperation' operations for 'ProgramCounter'
instance (Num addrType, Integral addrType, FiniteBits addrType) => PCOperation (ProgramCounter addrType) where
  pcInc pc                             = (+ 1) <$> pc
  pcDec pc                             = (+ (negate 1)) <$> pc
  pcDisplace (RelativePC disp) (PC pc) = PC (pc + signExtend disp)
