-- | Re-export module for 'Machine'-related code.

module Machine
       ( module Machine.EmulatedProcessor
       , module Machine.DisassemblerTypes
       , module Machine.NullProcessor
       , module Machine.Utils
       , CommonEmulatorOptions(..)
       , defaultCommonEmulatorOptions
       ) where

import Machine.EmulatedProcessor
import Machine.DisassemblerTypes
import Machine.NullProcessor
import Machine.Utils

-- | Common command line option data record
data CommonEmulatorOptions =
  EmulatorOptions
  { emulator :: String                  -- ^ The processor emulator's name
  }

-- | The default emulator options
defaultCommonEmulatorOptions :: CommonEmulatorOptions
defaultCommonEmulatorOptions = EmulatorOptions
                         { emulator = ""
                         }
