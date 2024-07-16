SOURCES = syntax.ml parser.mly lexer.mll while_lang.ml
RESULT = while_lang

all: byte-code native-code

-include OCamlMakefile
