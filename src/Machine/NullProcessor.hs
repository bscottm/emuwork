-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import Data.Word
import qualified Data.Vector.Unboxed as DVU

import Machine.EmulatedSystem

-- | There is no machine state for this processor.
data NullProcState = NullProcState

instance GenericPC NullProcState where
  pcInc pc = pc
  pcDec pc = pc
  pcDisplace _ pc = pc

nullProcessor :: EmulatedSystem NullProcState NullProcState Word32 Word32
nullProcessor = EmulatedSystem
                { _processor = EmulatedProcessor
                               { _procPrettyName = "Null (dummy) processor"
                               , _procAliases    = ["null", "dummy"] 
                               , _internals      = NullProcState
                               }
                , _memory    = MemorySystem
                               { _memInternals = NullProcState
                               , _mfetch       = (\_addr         -> 0 :: Word32)
                               , _mfetchN      = (\_addr _nBytes -> DVU.empty)
                               , _maxmem       = 0 :: Word32
                               }
                }

-- | 'EmuCommandLineDispatch' type family instance for the null processor
instance EmuCommandLineDispatch NullProcState where
  cmdDispatch _state options = putStrLn $ "Null processor dispatch invoked, args = " ++ (show options)
