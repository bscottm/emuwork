{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | General data structures and type classes for emulated processors.

module Machine.EmulatedProcessor where

import Control.Lens

-- | 'EmulatedProcessor' encapsulates general information about an emulated machine.
data EmulatedProcessor procInternals =
  EmulatedProcessor
  { _procPrettyName :: String                    -- ^ Pretty name for the emulated processor
  , _procAliases    :: [String]                  -- ^ Other names by which this processor is known
  -- | Processor-specific internal data.
  , _internals      :: procInternals
  }

-- Emit Template Haskell hair for the lenses
makeLenses ''EmulatedProcessor

-- | Identify this emulated processor
procIdentify :: EmulatedProcessor procInternals
             -> String
             -> Bool
procIdentify theProc name = theProc ^.  procAliases ^& (name `elem`)

-- | Abstract interface to functions that a processor emulation should provide.
class EmulatorActions wordType addrType dispType procInternals where
  -- | The machine's program counter function
  pcStep :: addrType                            -- ^  The current program counter
         -> PCAction addrType dispType          -- ^  Action to perform
         -> addrType                            -- ^  Resulting program counter after action

  -- The default implementation for pcStep, which should work for a broad cross section
  -- of types.
  pcStep addr Inc      = addr + 1
  pcStep addr Dec      = addr - 1
  pcStep addr (Disp x) = addr + (fromIntegral x)
  pcStep _    (Abs x)  = x

-- | Program counter actions: 'Inc' to increment by one, 'Dec' to decrement by one,
-- and 'Disp' @x@ to displace the program counter by @x@ units (bytes, words, etc.)
data PCAction addrType dispType where
  Inc  :: (Integral addrType) =>
          PCAction addrType dispType           -- Increment the program counter
  Dec  :: (Integral addrType) =>
          PCAction addrType dispType           -- Decrement the program counter
  Disp :: (Integral addrType, Integral dispType) =>
          dispType
       -> PCAction addrType dispType            -- Displace the program counter by multiple machine words.
  Abs  :: (Integral addrType) =>
          addrType
       -> PCAction addrType dispType            -- Absolute address -> program counter

-- | Emulator command line interface type class. This separates out the handling from the 'EmulatedProcessor'
-- processor internals, reducing the amount of polymorphic magic.
class EmuCommandLineDispatch procInternals where
  cmdDispatch    :: EmulatedProcessor procInternals
                 -> [String]
                 -> IO ()
