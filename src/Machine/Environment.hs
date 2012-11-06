module Machine.Environment
       ( CmdEnvironment(..)
       , CmdEnvDispatch(..)
       , defaultCmdEnvironment
       ) where

-- Redundant imports:
-- import System.FilePath
-- import Data.Maybe

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
            { emulator :: Maybe EmulatedProcessor -- ^ The emulator
            , emuImage :: FilePath                -- ^ An image to load, e.g., a ROM or bootstrap code
            }
            
-- | Default command environment values
defaultCmdEnvironment :: CmdEnvironment
defaultCmdEnvironment = CmdEnvironment
               { emulator = defaultEmulator
               , emuImage = defaultEmuImage
               }
  where
    defaultEmulator = Nothing
    defaultEmuImage = ""