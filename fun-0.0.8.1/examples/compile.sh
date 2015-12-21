#!/bin/bash
read -p "Enter a file name for compilation: " fn
cat $fn | ../src/ParseHM > extra/parsed.lam
cat extra/parsed.lam |  ../src/hm2cr > extra/translated.lam
cat extra/translated.lam | ../src/PpCore > extra/printed.tcrr
uhcr --corerunopt=printresult extra/printed.tcrr
./compile.sh