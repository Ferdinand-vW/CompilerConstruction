Compiler Construction - Mini Project F: Monotone Framework assignment

Ferdinand van Walree 3874389 and Matthew Swart 5597250

This project uses the following compiler verions:
- Haskell version: 7.10.2-a

To build the package it is necessary to to follow these instructions:
- Make sure uuagc, alex and happy are installed:
* cabal install uuagc
* cabal install alex
* cabal install happy
- cd src
- ./build.sh
- Code should now have been compiled and be loaded into ghci

Modules:

View
View.PpAdministration -> In this module you can find the implementation of the administration output
View.PpAnalysis -> In this module you can find the implementation of the the display for the general output
View.PpConstantPropagation -> In this module you can find the implementation of the display for the Constant Propagation
View.PpEmbellishedConstantPropagation -> In this module you can find the implementation of the display for the Embellished Constant Propagation.
View.PpHelper -> In this module you find the helper functions of the display.
View.PpLiveVariableAnalysis -> In this module you can find the implementation of the display for the live variable analysis.
View.View -> In this module you can find the implementation of the display types.

Monotome
Monotome.Analysis -> In this module you can find the worklist algorithm
Monotome.ConstantPropagation -> In this module you can find the implementation of constant propagation
Monotome.EmbellishedConstantPropagation -> In this module you find the implementation of embellished constant propagation
Monotome.LiveVariableAnalysis -> In this module you canfind the implementation of live varibales Analysis.
Monotome.MonotoneFramework -> In this module are the datatypes of the monotome framework and the transfer function.

Administration -> Contains generated AG code. Administration is used to gather info about the Program.

In the "example" directory we added several examples, which you can run in the program to analysis the examples. It is also possible to make your own examples.
To run the examples you can use the function "run", which needs 2 parameters. The first parameters is the analysis function and the second parameter is the location to the file.

Furthermore we have a "documentation" directory that contains a walkthrough of the strong live variable analysis and embellished constant propagation. The walkthrough explains the result of an analysis of a given program.

We have the following analysis functions:
cp -> constant propagation analysis
slv -> strong live variable analysis
ecp -> Embellished Constant Propagation analysis.


For example you could run something like this:
run slv "slv/slv1"
run cp "slv/cp1"
run (ecp 2) "cp/doublesquare" --2 here is the k value
