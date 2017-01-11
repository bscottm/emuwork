{-# LANGUAGE MultiParamTypeClasses #-}

-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import           Data.Word

import           Machine.MemorySystem
import           Machine.EmulatedSystem

-- | There is no machine state for this processor.
data NullProcState = NullProcState

type NullSystem = SimpleEmulatedSystem NullProcState Word32 Word32 NullProcState

nullProcessor :: NullSystem
nullProcessor = EmulatedSystem
                { _processor   = EmulatedProcessor
                                 { _procPrettyName = "Null (dummy) processor"
                                 , _internals      = NullProcState
                                 }
                , _memory      = initialMemorySystem
                , _sysName     = "Null/dummy system"
                , _sysAliases  = ["null", "dummy"]
                }

-- | Processor operations
instance ProcessorOps NullProcState Word32 Word32 Word32 Word32 where
  idecode pc mem = (DecodedInsn pc NullProcState, mem)

nullProcCmdDispatch :: SimpleEmulatedSystem procType addrType wordType insnSet
                    -> [String]
                    -> IO ()
nullProcCmdDispatch _emu options = putStrLn ("Null processor dispatch invoked, args = " ++ show options)
