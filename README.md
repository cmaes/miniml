# Prerequisties

## Install OCAML

On Linux

```
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install curl build-essential m4 ocaml opam
```

## Setup OPAM

```
opam init
eval `opam config env`
```

## Install Menhir

```
opam install menhir
```
## Install LLVM OCaml Bindings

```
sudo apt-get install llvm-3.7 llvm-3.7-dev llvm-3.7-runtime
opam install llvm
```

# Goals

This project is an experiment in compiling a subset of the ML language
(or more properly OCaml).  The project follows the structure of the
[mincaml compiler](esumii.github.io/min-caml/).  However, instead of
directly emiting assembler, we use the OCaml LLVM bindings to emit
LLVM IR. These bindings don't need to have the code in SSA form, meaning
that our intermediate language used inside the compiler does not need
to be in kNormal form.

# Closure conversion

The most complicated part of the compiler is the typed closure conversion.


# Outline

The compiler is structured as follows:

* lexer.mll - The lexer (use ocamllex to generate lexer.ml)
* paser.mly - The parser (use menhir to generate parser.ml)
* type.ml   - Defines the types supported in miniml
* id.ml     - Functions for working with identifiers
* syntax.ml - Defines the AST for miniml
* env.ml    - Defines the environment data structure
* s.ml      - Defines a set data structure
* typing.ml - Performs type inference
* inter.ml  - Defines an intermediate language, and converts from AST
* closure.ml - Performs closure conversion
* prettyprint.ml - Pretty printing routines
* compile.ml - Emit's LLVM IR
* miniml.ml - The main program driver

# Running

There is a small collection of sample programs written in miniml:
 * fac.mml
 * fib.mml
 * sum.mml
 * quad.mml
 * adder.mml
 * make_dbl.mml

You can compile these programs by doing `make progname.exe`. For
example, to compile fib.mml into an exectuable do:

```
make fib.exe
```

You can then run fib.exe via
```
./fib.exe
```
