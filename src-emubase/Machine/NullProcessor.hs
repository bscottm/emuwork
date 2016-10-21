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

type NullSystemT = EmulatedSystem NullProcState NullProcState Word32 Word32 NullProcState

nullProcessor :: NullSystemT
nullProcessor = EmulatedSystem
                { _processor  = EmulatedProcessor
                                { _procPrettyName = "Null (dummy) processor"
                                , _internals      = NullProcState
                                }
                , _memory     = MemorySystem
                                { _memInternals = NullProcState
                                , _mfetch       = (\_addr         -> 0 :: Word32)
                                , _mfetchN      = (\_addr _nBytes -> DVU.empty)
                                , _maxmem       = 0 :: Word32
                                }
                , _idecode    = (\pc _mem -> DecodedInsn pc NullProcState)
                , _sysName    = "Null/dummy system"
                , _sysAliases = ["null", "dummy"]
                }

-- | 'EmuCommandLineDispatch' type family instance for the null system
instance EmulatorDriver NullSystemT where
  formalName sys             = sys ^. sysName
  identityNames sys          = sys ^. sysAliases
  cmdDispatch _state options = putStrLn $ "Null processor dispatch invoked, args = " ++ (show options)