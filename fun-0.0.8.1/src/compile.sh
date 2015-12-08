#!/bin/bash
uuagc -Hdfcsw CCO/Core/AG.ag
uuagc -Hdfcsw CCO/HM/AG.ag
SLEEP 0.5
ghc --make ParseHM.hs
ghc --make hm2cr.hs
ghc --make PpCore.hs
