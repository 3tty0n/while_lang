SOURCES = syntax.ml parser.mly lexer.mll ssa.ml emit_stackmachine.ml emit_pyc.ml while_lang.ml
RESULT = while_lang

all: native-code

clean::
	$(RM) *.cmt *.cmti

-include OCamlMakefile
