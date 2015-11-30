#!/bin/bash
# To run this script it is required to have an unix bash, such as MINGW
# And We assume that the programs are compiled.
read -p "Enter a file name for compilation: " fn
echo $fn
cat $fn | ./ParseTDiag.exe > parsed.bl
cat parsed.bl | ./TypeCDiag.exe > typechecked.bl
cat typechecked.bl | ./TDiag2Picture.exe > translated.bl
cat translated.bl | ./ppPicture.exe > printed.bl
cat printed.bl

echo ""
echo "The output has been printed to printed.bl"
echo "Now copy paste the generated code into printed.tex, in between \\begin{document} and \\end{document}"