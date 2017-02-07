-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import           Machine.System

nullProcCmdDispatch :: EmulatedSystem NullCPU NullInsnSet addrType wordType
                    -> [String]
                    -> IO ()
nullProcCmdDispatch _emu options = putStrLn ("Null processor dispatch invoked, args = " ++ show options)
