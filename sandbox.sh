#!/bin/sh

cabal sandbox init
cabal sandbox add-source src-emubase src-trs80
cabal install --only-dependencies --enable-documentation
