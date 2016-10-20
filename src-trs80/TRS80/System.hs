module TRS80.System where

import Control.Lens

import TRS80.Memory

import Machine
import Z80

trs80ModelISystem =
  (z80generic ^. memory) .~ modelI16K
