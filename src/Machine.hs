-- | Re-export module for 'Machine'-related code.

module Machine
       ( module Machine.EmulatedProcessor
       , module Machine.NullProcessor
       , CommonEmulatorOptions(..)
       , defaultCommonEmulatorOptions
       ) where

import Machine.EmulatedProcessor
import Machine.NullProcessor

-- | Common command line option data record
data CommonEmulatorOptions =
  EmulatorOptions
  { emulator :: Maybe EmulatedProcessor         -- ^ The processor emulator
  }

-- | The default emulator options
defaultCommonEmulatorOptions :: CommonEmulatorOptions
defaultCommonEmulatorOptions = EmulatorOptions
                         { emulator = Nothing
                         }
