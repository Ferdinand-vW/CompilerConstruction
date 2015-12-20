cabal sandbox init
cabal install -j
(cd src && exec ./compile.sh)