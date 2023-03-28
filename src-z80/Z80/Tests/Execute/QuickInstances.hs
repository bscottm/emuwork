{-# OPTIONS_GHC -fno-warn-orphans #-}

module Z80.Tests.Execute.QuickInstances where

import           Test.QuickCheck                      (Arbitrary, arbitrary, shrink, elements)

import Z80

-- | Generate `Arbitrary` 8-bit registers for QuickCheck. Note that this does not
-- include indirect memory addresses, just simple registers.
instance Arbitrary Z80reg8 where
  arbitrary = elements [A, B, C, D, E, H, L, IXh, IXl, IYh, IYl]
  shrink A = [B, C, D, E, H, L, IXh, IXl, IYh, IYl]
  shrink B = [C, D, E, H, L, IXh, IXl, IYh, IYl]
  shrink C = [D, E, H, L, IXh, IXl, IYh, IYl]
  shrink D = [E, H, L, IXh, IXl, IYh, IYl]
  shrink E = [H, L, IXh, IXl, IYh, IYl]
  shrink H = [L, IXh, IXl, IYh, IYl]
  shrink L = [IXh, IXl, IYh, IYl]
  shrink IXh = [IXl, IYh, IYl]
  shrink IXl = [IYh, IYl]
  shrink IYh = [IYl]
  shrink IYl = []
  shrink _   = undefined
