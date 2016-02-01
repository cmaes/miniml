all: miniml.byte

OBJECTS = type.cmo id.cmo syntax.cmo lexer.cmo parser.cmo prettyprint.cmo \
          env.cmo s.cmo typing.cmo alpha.cmo closure.cmo inter.cmo

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

inter.cmo: inter.ml env.cmo syntax.cmo typing.cmo
	ocamlc -c $(FLAGS) inter.ml

closure.cmo: closure.ml inter.cmo s.cmo env.cmo syntax.cmo
	ocamlc -c $(FLAGS) closure.ml

prettyprint.cmo: prettyprint.ml closure.cmo inter.cmo syntax.cmo
	ocamlc -c $(FLAGS) prettyprint.ml

alpha.cmo: alpha.ml inter.cmo env.cmo syntax.cmo
	ocamlc -c $(FLAGS) alpha.ml

miniml.byte: miniml.ml $(OBJECTS)
	ocamlfind ocamlc $(FLAGS) -linkpkg $(OBJECTS) miniml.ml -o miniml.byte

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi
	-rm *.conflicts
	-rm *.byte
