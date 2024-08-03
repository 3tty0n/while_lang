SOURCES = syntax.ml parser.mly lexer.mll ssa.ml virtual_stack.ml virtual_pyc.mli virtual_pyc.ml assemble_pyc.mli assemble_pyc.ml while_lang.ml
RESULT = while_lang
PACKS =

all: native-code

clean::
	$(RM) *.cmt *.cmti

-include OCamlMakefile
