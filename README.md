# MiniML Goals

This project is an experiment in compiling a subset of the ML language
(or more properly [OCaml](https://ocaml.org)).  The project follows the structure of the
[mincaml compiler](https://esumii.github.io/min-caml/).  However, instead of
directly emiting assembler, we use the OCaml LLVM bindings to emit
[LLVM IR](https://llvm.org).
These bindings don't need to have the code in SSA form, meaning
that our intermediate language used inside the compiler does not need
to be in kNormal form.

## How to install MiniML
### Install OCAML

On Ubuntu or Debian, if you don't have OCaml and OPAM yet, you can do

```
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install curl build-essential m4 ocaml opam
```

or use your system's package manager and install its version of OCaml and OPAM.

### Setup OPAM

Install [OPAM](https://opam.ocaml.org)
```
opam init
eval `opam config env`
```

### Install [Menhir](https://opam.ocaml.org/packages/menhir/)

```bash
opam install menhir
```

### Install LLVM OCaml Bindings
On Ubuntu or Debian:

```bash
sudo apt-get install llvm-3.7 llvm-3.7-dev llvm-3.7-runtime
opam install llvm
```

## MiniML organization
### Closure conversion

The most complicated part of the compiler is the typed closure conversion.

### Outline

The compiler is structured as follows:

* [`lexer.mll`](lexer.mll) - The lexer (use ocamllex to generate `lexer.ml`)
* [`paser.mly`](paser.mly) - The parser (use menhir to generate `parser.ml`)
* [`type.ml`](type.ml)   - Defines the types supported in miniml
* [`id.ml`](id.ml)     - Functions for working with identifiers
* [`syntax.ml`](syntax.ml) - Defines the AST for miniml
* [`env.ml`](env.ml)    - Defines the environment data structure
* [`s.ml`](s.ml)      - Defines a set data structure
* [`typing.ml`](typing.ml) - Performs type inference
* [`inter.ml`](inter.ml)  - Defines an intermediate language, and converts from AST
* [`closure.ml`](closure.ml) - Performs closure conversion
* [`prettyprint.ml`](prettyprint.ml) - Pretty printing routines
* [`compile.ml`](compile.ml) - Emit's LLVM IR
* [`miniml.ml`](miniml.ml) - The main program driver

## Examples of MiniML programs

There is a small collection of sample programs written in miniml:

 * [`fac.mml`](fac.mml)
 * [`fib.mml`](fib.mml)
 * [`sum.mml`](sum.mml)
 * [`quad.mml`](quad.mml)
 * [`adder.mml`](adder.mml)
 * [`make_dbl.mml`](make_dbl.mml)

You can compile these programs by doing `make progname.exe`. For
example, to compile [`fib.mml`](fib.mml) into an exectuable do:

```bash
make fib.exe
```

You can then run `fib.exe` via
```bash
./fib.exe
```
