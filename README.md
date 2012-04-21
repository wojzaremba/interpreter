Requirements
============
Project require
- haskell
- bnfc
- happy
- alex 

Overview
========
Project contains simple code interpreter developed in haskell. File Ast.cl contains description of a grammar. In order to complile entire project please execute make file. All tests are available in examples directory. Test procedure might be start by executing start.sh script. Project is based on syntax from JavaLight programming language.

Capabilities
============
This interpreter supports 
- basic constructions like
  - function calls
  - loops - while and for
  - if-statements
- statements as expressions
- post/pre incrementation, decrementation (++, --)
- data types like boolean, int, double
- lazy evaluation of boolean expressions (in expression a && b, b is called only if a is false)
- arrays 
  - dynamic size allocation
  - passing arrays by reference
  - multi-dimensional arrays
- type casting (int <-> bool <-> double)


Author
======
Developed by Wojciech Zaremba. You can reach me at woj.zaremba (at) gmail (dot) com
