{-# LANGUAGE FlexibleInstances #-}
module Z80.CmdDispatch where

import Machine.Environment

instance CmdEnvDispatch z80processor where
  cmdDispatch = z80cmdDispatch
  
-- | Various things we can do with the Z80 processor emulator
data Z80Commands =
  Disassemble

-- | The Z80 command line interface dispatch function
z80cmdDispatch machine cmdEnv options =
  putStrLn "Z80 processor dispatch"