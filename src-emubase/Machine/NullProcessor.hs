{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import           Control.Lens
import qualified Data.Vector.Unboxed as DVU
import           Data.Word

import           Machine.EmulatedSystem
import           Machine.EmulatorDriver

-- | There is no machine state for this processor.
data NullProcState = NullProcState

instance GenericPC NullProcState where
  pcInc pc = pc
  pcDec pc = pc
  pcDisplace _ pc = pc

type NullSystemT = EmulatedSystem NullProcState Word32 Word32 NullProcState

nullProcessor :: NullSystemT
nullProcessor = EmulatedSystem
                { _processor  = EmulatedProcessor
                                { _procPrettyName = "Null (dummy) processor"
                                , _internals      = NullProcState
                                }
                , _memory     = MemorySystem NullMemorySystem
                , _sysName    = "Null/dummy system"
                , _sysAliases = ["null", "dummy"]
                }

-- | The null memory system
data NullMemorySystem where
  NullMemorySystem :: NullMemorySystem

-- | Memory operations on a null memory system
instance MemoryOps NullMemorySystem Word32 Word32 where
  mFetch _msys _addr          = 0
  mFetchN _msys _addr _nbytes = DVU.empty

-- | Processor operations
instance ProcessorOps NullProcState Word32 Word32 where
  idecode pc _mem = DecodedInsn pc NullProcState

-- | 'EmuCommandLineDispatch' type family instance for the null system
instance EmulatorDriver NullSystemT where
  formalName sys             = sys ^. sysName
  identityNames sys          = sys ^. sysAliases
  cmdDispatch _state options = putStrLn ("Null processor dispatch invoked, args = " ++ show options)
