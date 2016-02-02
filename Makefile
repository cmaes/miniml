all: miniml.byte

OBJECTS = type.cmo id.cmo syntax.cmo lexer.cmo parser.cmo prettyprint.cmo \
          env.cmo s.cmo typing.cmo inter.cmo alpha.cmo closure.cmo compile.cmo

LLVM_MODULES = llvm,llvm.analysis,llvm.executionengine,llvm.target,llvm.scalar_opts

FLAGS = -g

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	menhir --explain parser.mly

type.cmo: type.ml
	ocamlc -c $(FLAGS) type.ml

id.cmo: id.ml type.cmo
	ocamlc -c $(FLAGS) id.ml

syntax.cmo: syntax.ml type.cmo id.cmo
	ocamlc -c $(FLAGS) syntax.ml

parser.cmo: parser.mli parser.ml syntax.cmo
	ocamlc -c $(FLAGS) parser.mli
	ocamlc -c $(FLAGS) parser.ml

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $(FLAGS) lexer.ml

env.cmo: env.ml id.cmo
	ocamlc -c $(FLAGS) env.ml

s.cmo: s.ml id.cmo
	ocamlc -c $(FLAGS) s.ml

typing.cmo: typing.ml env.cmo
	ocamlc -c $(FLAGS) typing.ml

inter.cmi: inter.mli syntax.cmo
	ocamlc -c $(FLAGS) inter.mli

closure.cmi: closure.mli inter.cmi
	ocamlc -c $(FLAGS) closure.mli

prettyprint.cmo: prettyprint.ml closure.cmi inter.cmi syntax.cmo
	ocamlc -c $(FLAGS) prettyprint.ml

inter.cmo: inter.ml env.cmo syntax.cmo typing.cmo prettyprint.cmo
	ocamlc -c $(FLAGS) inter.ml

alpha.cmo: alpha.ml inter.cmo env.cmo syntax.cmo
	ocamlc -c $(FLAGS) alpha.ml

closure.cmo: closure.ml inter.cmo s.cmo env.cmo syntax.cmo
	ocamlc -c $(FLAGS) closure.ml

compile.cmo: compile.ml closure.cmo id.cmo
	ocamlfind	ocamlc -c $(FLAGS) -package llvm compile.ml

miniml.byte: miniml.ml $(OBJECTS)
	ocamlfind ocamlc $(FLAGS) -package $(LLVM_MODULES) -linkpkg $(OBJECTS) miniml.ml -o miniml.byte

%.ll: %.mml miniml.byte
	./miniml.byte $<

%.bc: %.ll
	llvm-as-3.7 $<

%.s: %.bc
	llc-3.7 $<

runtime.o: runtime.c
	clang-3.7 -c runtime.c

%.o: %.s
	clang-3.7 -c $<

%.exe: %.o runtime.o
	clang-3.7 -o $@ $^

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi
	-rm *.conflicts
	-rm *.byte
	-rm *.ll *.bc *.s *.o *.exe
