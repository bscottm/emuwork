{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import Data.Word
import Data.Int
import Machine.EmulatedProcessor

-- | There is no machine state for this processor.
data NullProcState = NullProcState

-- | Instantiate the NullProcessor's emulator actions:
instance EmulatorActions Word32 Word32 Int32 NullProcState where
  pcStep addr _action = addr

nullProcessor :: EmulatedProcessor NullProcState
nullProcessor = EmulatedProcessor { _procPrettyName = "Null (dummy) processor"
                                  , _procAliases    = ["null", "dummy"] 
                                  , _internals = NullProcState
                                  }

-- | 'EmuCommandLineDispatch' type family instance for the null processor
instance EmuCommandLineDispatch NullProcState where
  cmdDispatch _state options = putStrLn $ "Null processor dispatch invoked, args = " ++ (show options)
