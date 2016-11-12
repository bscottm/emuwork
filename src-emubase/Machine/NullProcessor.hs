{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import           Data.Word

import           Machine.MemorySystem
import           Machine.EmulatedSystem

-- | There is no machine state for this processor.
data NullProcState = NullProcState

type NullSystem = EmulatedSystem NullProcState Word32 Word32 NullProcState

nullProcessor :: NullSystem
nullProcessor = EmulatedSystem
                { _processor   = EmulatedProcessor
                                 { _procPrettyName = "Null (dummy) processor"
                                 , _internals      = NullProcState
                                 }
                , _memory      = initialMemorySystem
                , _sysName     = "Null/dummy system"
                , _sysAliases  = ["null", "dummy"]
                , _cmdDispatch = nullProcCmdDispatch
                }

-- | Processor operations
instance ProcessorOps NullProcState Word32 Word32 where
  idecode pc _mem = DecodedInsn pc NullProcState

nullProcCmdDispatch :: EmulatedSystem procType addrType wordType insnSet
                    -> [String]
                    -> IO ()
nullProcCmdDispatch _emu options = putStrLn ("Null processor dispatch invoked, args = " ++ show options)
