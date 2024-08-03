SOURCES = syntax.ml parser.mly lexer.mll ssa.ml emit_stackmachine.ml emit_pyc.ml assemble_pyc.ml while_lang.ml
RESULT = while_lang
PACKS =

all: native-code

clean::
	$(RM) *.cmt *.cmti

-include OCamlMakefile
