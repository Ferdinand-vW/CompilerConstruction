#!/bin/bash
read -p "Enter a file name for compilation: " fn
cat $fn | ./ParseHM.exe > ../examples/parsed.lam
cat ../examples/parsed.lam
cat ../examples/parsed.lam | ./hm2cr.exe > ../examples/translated.lam
cat ../examples/translated.lam
cat ../examples/translated.lam | ./PpCore.exe > ../examples/printed.tcrr
cat ../examples/printed.tcrr
uhcr --corerunopt=printresult ../examples/printed.tcrr
echo "The output can be found in ../examples/printed.tcrr"