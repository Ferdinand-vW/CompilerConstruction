Compiler Construction Project T-diagram

Ferdinand van Walree 3874389 and Matthew Swart 5597250

This project uses the following compiler verions:
- Haskell version: 7.10.2-a
- uuagc-0.9.52.1
To build the package it is necessary to execute the following instructions:
- Type in the console: "make"
- Goto the "src" folder and type in the console "./build.sh"

The design/documentation of our code and project can be found in the documentation folder.
That includes a formal specification of our typesystem and the general
documentation of our implementation of generating T-diagrams. Which includes the design, implementation and
a description of our type system. 

The generated haddock can be found at "dist\doc\html\CCO3874389N5597250CCO".

To run our compiler you must first have completed the above steps. Then you can use the following command:
"./compile.sh". This will prompt you for a file name. We have prepared 13 example inputs: example1-4.bl and error1-9.bl.
These are located in the same folder, so just supplying the name with extension should
be enough. When you compiled one of these inputs, the generated output will be located in printed.bl. Now you have to
copy-paste what is inside printed.bl into printed.tex. Make sure to do so within the \begin{document} and \end{document}
tags. You will need a program that can run a tex file, such as TexWorks. The output is generated in landscape mode. We
also added a frame to show how the diagram neatly fits within the frame.