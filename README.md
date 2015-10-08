## Overview
SSIfy is a parameterized tool for constructing program representations suitable for program static analyses. Given an input program, it can be converted to forms such as Extended Static Single Assignment, Static Single Use, etc.

The main motivation is to offer an all-in-one solution, which is capable of producing program representations that combine properties of different forms. That is, one can transform a program to a representation which is, loosely speaking, e-SSA and SSU at the same time.

Current version: 2.0.

## Compatibility

SSIfy works on LLVM 3.7. Along with installing SSIfy, it's necessary to deploy a modified version of LLVM's Dominators.h, also provided here.

## Modes of operation

SSIfy has two modes of operation: upwards and downwards. These should be selected according to the purpose of the analysis that will follow the transformation. That is, if the analysis flows information down (e.g. tainted flow analysis, range analysis), you should set the downwards mode on. Likewise, if the analysis flows information up (e.g. class inference), you should set the upwards mode on.

In conjunction with the flow direction option, you can also specify where the live range splitting should occur. The two options are (i) in the exit of conditional branches and (ii) after uses of values.

## Implementation
This is done as a compiler pass that works on LLVM 3.7, written in C++.

## Requirements
In order to use SSIfy on LLVM bitcode programs, one _has_ to run it through the following passes first:

1. mem2reg;

2. break-crit-edges;

Also, for cosmetic reasons, it is recommended to run _instnamer_ pass after all passes that create new variables.

## How to run
After building LLVM and SSIfy, you can try it with the command:

$ opt -mem2reg -break-crit-edges [... -instnamer] -load /path/to/SSIfy.so -ssify [-v] _FLAGS_ Input.bc -o Output.bc

-v: verbose mode

## Further

If you have any questions or suggestions, I'd be happy to address them. Reach out on victorsc at dcc.ufmg.br
