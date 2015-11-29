#!/bin/bash
cabal install -j
ghc --make src/ParseTDiag.hs
ghc --make src/TypeCDiag.hs
ghc --make src/TDiag2Picture.hs
ghc --make src/ppPicture.hs