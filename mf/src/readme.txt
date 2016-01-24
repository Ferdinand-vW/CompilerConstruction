This project uses the following compiler verions:
- Haskell version: 7.10.2-a
- uuagc-0.9.52.1
To build the package it is necessary to follow the following instructions:
- Go to the src directory in a bash console; In this directory there is a file called "Build.sh"
- Type in the console "./Build.sh". This script compiles all the necesarry code.

--Module structure:
Administration
All
Analysis
ConstantPropagation
EmbellishedConstantPropagation
LiveVariableAnalysis
MonotoneFramework

View
PpAdministration
PpAnalysis
PpConstantPropagation
PpEmbellishedConstantPropagation
PpHelper
PpLiveVariableAnalysis
View

Monotome
Analysis
ConstantPropagation
EmbellishedConstantPropagation
LiveVariableAnalysis
MonotoneFramework



Exercise 5.
In the "example" directory we added several examples, which you can run in the program to analyse the examples. It is also possible to make your own examples.
To run the examples you need to use the function "run", which needs 2 parameters. The first parameters is the analyse function and the second parameters is the location to the file.

We have the following analyse function:
cp -> constant propagation analyse
slv -> strong live variable analyse
ecp -> Embellished Constant Propagation analyse.

For example you could run something like this:
run slv "slv/slv1"
run cp "slv/cp1"
run (ecp 2) "example6"