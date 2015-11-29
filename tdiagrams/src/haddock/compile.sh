#!/bin/bash
# My first script
# To run this script it is required to have an unix bash, such as MINGW
# And We assume that the programs are compiled. 
cat in.bl | ./ParseTDiag.exe > parsed.bl
cat parsed.bl | ./TypeCDiag.exe > typechecked.bl
cat typechecked.bl | ./TDiag2Picture.exe > translated.bl
cat translated.bl | ./ppPicture.exe > printed.bl
cat printed.bl