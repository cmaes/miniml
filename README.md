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

# Goals

A simple experiment in compiling a minimal ML-type language.

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
