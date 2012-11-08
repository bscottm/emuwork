{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-|
  General data structures and type classes for emulated processors.
-}

module Machine.EmulatedProcessor where

{- |
  An abstract machine's program counter: given a memory address and an action,
  produce a new memory address. This avoids having to store the current program
  counter and gives some flexibility on how to manipulate the counter (Does the
  PC increment by bytes? words? quuxes?)
-}
type ProgramCounterF addrType dispType =
  ( addrType                      --  The current program counter
    -> PCAction addrType dispType --  Action to perform
    -> addrType                   --  Resulting program counter after action
  )

{-|
  'EmulatedProcessor' encapsulates general information about an emulated machine.
-}
data EmulatedProcessor =
  forall machineSpecific . EmulatedProcessor
  { machineName    :: String          -- ^ Identifying name of the machine
  , names          :: [String]        -- ^ List of names for the emulated processor
  , cmdDispatch    :: machineSpecific
                   -> [String]
                   -> IO ()
  -- | Machine-specific data.
  --
  --  Note: This is existentially qualified to make this record intentionally polymorphic.
  , internals      :: machineSpecific
  }

{- |
  Abstract interface to functions that a processor emulation should provide.
-}
class EmulatorActions wordType addrType dispType machineSpecific where
  -- | The machine's program counter function
  programCounter :: ProgramCounterF addrType dispType 
  -- | Step one instruction.
{-
  stepOne :: machineSpecific
          -> machineSpecific
-}
  -- | Disassemble a stream of instructions
  
{- |
  Program counter actions: 'Inc' to increment by one, 'Dec' to decrement by one,
  and 'Disp' @x@ to displace the program counter by @x@ units (bytes, words, etc.)
-} 
data PCAction addrType dispType where
  Inc  :: PCAction addrType dispType             -- Increment the program counter
  Dec  :: PCAction addrType dispType             -- Decrement the program counter
  Disp :: dispType -> PCAction addrType dispType -- Displace the program counter by multiple machine words.
  Abs  :: addrType -> PCAction addrType dispType -- Absolute address -> program counter
