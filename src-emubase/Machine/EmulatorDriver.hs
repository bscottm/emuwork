module Machine.EmulatorDriver where

-- | Emulator command line interface driver, responsible for identifying a system
-- and dispatching the command line.
class EmulatorDriver emulator where
  -- | The formal name of the system
  formalName    :: emulator
                -> String
  -- | The system's names used to identify it.
  identityNames :: emulator
                -> [String]
  -- | Command line dispatch
  cmdDispatch   :: emulator
                -> [String]
                -> IO ()
