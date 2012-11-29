{-# LANGUAGE MultiParamTypeClasses #-}

-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import Data.Word
import Data.Int
import Machine.EmulatedProcessor

-- | There is no machine state for this processor.
data NullProcState = NullProcState

-- | Instantiate the NullProcessor's emulator actions:
instance EmulatorActions Word32 Word32 Int32 NullProcState where
  programCounter addr _action = addr

nullProcessor :: EmulatedProcessor
nullProcessor = EmulatedProcessor { machineName = "Null (dummy) processor"
                                  , names = ["null", "dummy"]
                                  , cmdDispatch = nullCmdDispatch
                                  , internals = NullProcState
                                  }

nullCmdDispatch :: NullProcState
                -> [String]
                -> IO ()
nullCmdDispatch _state options =
  putStrLn $ "Null processor dispatch invoked, args = " ++ (show options)
