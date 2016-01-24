Compiler Construction - Mini Project F: Monotone Framework assignment

Ferdinand van Walree 3874389 and Matthew Swart 5597250

This project uses the following compiler verions:
- Haskell version: 7.10.2-a
- uuagc-0.9.52.1
To build the package it is necessary to follow the following instructions:
- Go to the src directory in a bash console; In this directory there is a file called "Build.sh"
- Type in the console "./Build.sh". This script compiles all the necesarry code.



View
View.PpAdministration -> In this module you can find the implementation of the administration output
ViewPpAnalysis -> In this module you can find the implementation of the the display for the general output
View.PpConstantPropagation -> In this module you can find the implementation of the display for the Constant Propagation
View.PpEmbellishedConstantPropagation -> In this module you can find the implementation of the display for the Embellished Constant Propagation.
View.PpHelper -> In this module you find the helper functions of the display.
View.PpLiveVariableAnalysis -> In this module you can find the implementation of the display for the live variable analysis.
View.View -> In this module you can find the implementation of the display types.

Monotome
Monotome.Analysis -> In this module you can find the worklist algorithm
Monotome.ConstantPropagation -> In this module you can find the implementation of the constant propagation
Monotome.EmbellishedConstantPropagation -> In this module you find the implementation of the constant propagation
Monotome.LiveVariableAnalysis -> In this module you canfind the implementation of the live varibales Analysis.
Monotome.MonotoneFramework -> In this module are the datatypes of the monotome framework and the transform function.


Exercise 5.
In the "example" directory we added several examples, which you can run in the program to analysis the examples. It is also possible to make your own examples.
To run the examples you can use the function "run", which needs 2 parameters. The first parameters is the analysis function and the second parameters is the location to the file.

We have the following analyse function:
cp -> constant propagation analysis
slv -> strong live variable analysis
ecp -> Embellished Constant Propagation analysis.

We have
- intraprocedural
- 

For example you could run something like this:
run slv "slv/slv1"
run cp "slv/cp1"
run (ecp 2) "examples6"