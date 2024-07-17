SOURCES = syntax.ml parser.mly lexer.mll emit.ml while_lang.ml
RESULT = while_lang

all: native-code

-include OCamlMakefile
