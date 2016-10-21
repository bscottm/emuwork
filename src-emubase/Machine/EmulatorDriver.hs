module Machine.EmulatorDriver where

import Machine.EmulatedSystem

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- | Emulator command line interface type class. This separates out the handling from the 'EmulatedProcessor'
-- processor internals, reducing the amount of polymorphic magic.
class EmulatorDriver procInternals addrType instructionSet where
  cmdDispatch    :: EmulatedProcessor procInternals addrType instructionSet
                 -> [String]
                 -> IO ()
