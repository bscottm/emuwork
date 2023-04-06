module Main where

import qualified Data.Text                      as T
import qualified Data.Vector.Unboxed            as DVU

import           Lens.Micro.Platform            ((&), (.~), (^.))

import           Machine                        (ProgramCounter (PC), ShowHex (as0xHex), decodedInsn, decodedInsnPC, idecode,
                                                 memory, mkROMRegion, processor, processorOps, sysAliases, sysName)

import           System.IO

import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@?=))

import           Z80
import           Z80.Tests.InstData             (InstTestCase (..), InstTestGroup (..), testOrigin, z80InstData)

main :: IO ()
main = defaultMain [ mkTestGroup test | test <- z80InstData ]
  where
    mkTestGroup tgrp = testGroup (groupDescription tgrp) (groupTests tgrp)
    groupTests  tgrp = [ testCase (instDescription tc) (z80instTest (instBytes tc) (instruction tc)) | tc <- testCases tgrp]

z80system :: Z80system Z80BaseSystem
z80system = z80generic & sysName .~ "Test Z80 generic system"
                       & sysAliases .~ []

z80instTest :: [Z80byte] -> Z80instruction -> Assertion
z80instTest bytes inst = diag >> (decodedPC @?= expectedPC) >> (inst' ^. decodedInsn @?= inst)
  where
    decodedPC      = inst' ^. decodedInsnPC
    expectedPC     = PC (testOrigin + fromIntegral (length bytes))
    (inst', _sys') = (testsys ^. processor . processorOps . idecode) (PC testOrigin) testsys
    testsys        = z80generic & memory .~ mkROMRegion testOrigin (DVU.fromList bytes) mempty
    diag
      | decodedPC /= expectedPC
      = hFlush stderr
        >> hPutStrLn stderr (T.unpack . T.concat $
              ["decodedPC != expectedPC, got "
              , as0xHex decodedPC
              , ", expected "
              , as0xHex expectedPC
              ])
        >> hFlush stderr
      | inst' ^. decodedInsn /= inst
      = hFlush stderr
        >> hPutStrLn stderr ("Bad decode, got " ++ show (inst' ^. decodedInsn) ++ ", expected " ++ show inst)
        >> hFlush stderr
      | otherwise
      = return ()
