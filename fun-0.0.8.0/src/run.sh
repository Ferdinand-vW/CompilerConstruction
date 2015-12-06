#!/bin/bash
read -p "Enter a file name for compilation: " fn
echo $fn
cat $fn | ./ParseHM.exe > parsed.bl
cat parsed.bl
cat parsed.bl | ./hm2cr.exe > translated.bl
cat translated.bl
cat translated.bl | ./PpCore.exe > printed.bl
cat printed.bl