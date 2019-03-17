# Fli/o
> Fli/o is a programming language for easily working with file I/O

The idea behind Fli/o is to simplify the process of working with files-- this includes everything from moving / copying files to reading and writing to files.

Most of this functionality is offered through the standard library of built-in functions included in this project.

**NOTE**: The language reference manual for Fli/o can be found [here](http://www.cs.columbia.edu/~sedwards/classes/2018/4115-fall/lrms/Fli-O.pdf).

Features:
- Primitive data types (e.g. int, char, string)
- For loops, if/else statements
- Functions
- Arithmetic operators (e.g. +, -, \*, /)
- Logical operators (e.g. <, >, ==, !=)
- Standard library of built-in file I/O functions

## Pipeline

Here is a brief explanation of the compilation pipeline for this project.

1. A Fli/o program is passed through the scanner (`scanner.mll`) where it's broken up into "tokens" or words (if it's easier to think of that way) separated by whitespace.
2. These tokens get passed into the parser (`parser.mly`) which generates an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)(AST) based on `ast.ml`. This is basically a tree representation of the program.
3. The AST is checked for semantic integrity via `semant.ml` and if any syntax or type errors are detected, the program will fail to compile.
4. The LLVM code is generated from the AST via `codegen.ml`.

The compiler for Fli/o is designed to compile to LLVM IR because whole idea behind the LLVM project is to have an intermediate representation of code that can be compiled down to architecture-specific machine code.

## Installation

[LLVM](https://en.wikipedia.org/wiki/LLVM), [OCaml](https://ocaml.org/) (including ocamlfind / ocamlbuild), and cc are required to build the front-end and back-end of the compiler for Fli/o.

## Usage

To build the compiler, simply run `make` from the src directory.

To compile your program written in Fli/o, run the following commands:

```sh
# Step 1. Compile Fli/o into LLVM IR
$flio.native < my_program.f > myprogram.s
# Step 2. Generate Assembly code from LLVM IR
$llc my_program.ll
# Step 3. Link in the library of Fli/o built-in functions and generate an executable
$clang my_program.s stdlib.c -o my_program
# Step 4. Run your program
$./my_program
```

**Note:** You can use other C compilers instead of clang for step 3 (e.g. gcc, cc)

## Testing

Fli/o contains an extensive test suite (located under the test directory) to ensure that all of the features of the language work as expected.

To run all tests, simply run `bash testall.sh`.

## Meta

Distributed under the GNU GPL v3.0 license. See ``LICENSE`` for more information.

## Contributing

1. [Fork](https://github.com/matthewachan/flio/fork) the repo
2. Create a feature branch (e.g. `git checkout -b feature/new_feature`)
3. Add & commit your changes
4. Push to your feature branch (e.g. `git push origin feature/new_feature`)
5. Create a new pull request
