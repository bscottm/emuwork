{- |
   The 'Machine.CmdEnvironment' module: Data types, type classes and defaults for a machine's command environment.
   This is just a way of driving a processor emulator, in which the common options are captured.
-}
module Machine.CmdEnvironment
       ( CmdEnvironment(..)
       , CmdEnvDispatch(..)
       , defaultCmdEnvironment
       ) where

import Machine.EmulatedProcessor

-- | Type class used to dispatch commands within a command environment, e.g.,
-- disassemble a ROM image.
class CmdEnvDispatch emulator where
  -- | The command dispatch function
  cmdDispatch :: emulator       -- ^ The emulated processor
              -> CmdEnvironment -- ^ The command environment
              -> [String]       -- ^ Command and its options
              -> IO ()

-- | Environment "stuff" extracted from the command line.
--
-- Note: This could also be used for a test harness...
data CmdEnvironment = CmdEnvironment
            { emulator  :: Maybe EmulatedProcessor -- ^ The emulator
            }
            
-- | Default command environment values
defaultCmdEnvironment :: CmdEnvironment
defaultCmdEnvironment = CmdEnvironment
               { emulator  = Nothing
               }
