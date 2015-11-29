#!/bin/bash
ghc --make ParseTDiag.hs
ghc --make TypeCDiag.hs
ghc --make TDiag2Picture.hs
ghc --make ppPicture.hs