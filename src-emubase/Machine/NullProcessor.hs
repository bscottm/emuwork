-- | An minimal definition for an emulator, the 'null' processor
module Machine.NullProcessor where

import           Data.Word

import           Machine.Types

nullProcCmdDispatch :: EmulatedSystem NullCPU NullInsnSet addrType wordType
                    -> [String]
                    -> IO ()
nullProcCmdDispatch _emu options = putStrLn ("Null processor dispatch invoked, args = " ++ show options)
