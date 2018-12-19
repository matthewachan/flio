# Fli/o
A programming language developed to create a seamless way for users to interact with files.

# Installation

## Prerequisites

LLVM, OCaml and cc are required to build the compiler for Fli/o.

## Steps

Run `make` in the /src directory. Then, to compile a program named `myprogram.f`, run `flio.native < myprogram.f > myprogram.s` to build the LLVM IR.

Then run `llc myprogram.ll` to build the Assembly code. Finally, run `clang myprogram.s stdlib.c -o myprogram` to link the Assembly code with Fli/o standard library and create an executable named `myprogram`.

Note, you can use other C compilers instead of clang here. (Ex: gcc, cc)

# Testing

To run all tests, run `bash testall.sh`.
