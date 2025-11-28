# もし実装を改良したら SOURCES にファイルを足す
SOURCES = syntax.ml parser.mly lexer.mll virtual_stack.ml \
	emit_wasm.ml visualizer.ml main.ml
# テストを追加したらテストの名前 (拡張子をは取る) を TESTS に足す
TESTS = assign loop
# 最終的に生成されるバイナリ名
RESULT = while_lang

all: bc # test

test: bc $(TESTS:%=test/%.res)

clean::
	$(RM) *.cmt *.cmti **/*.pyc **/*.res

clean_test:
	$(RM) **/*.pyc **/*.res

test/%.pyc: test/%.while
	./$(RESULT) test/$*.while

-include OCamlMakefile
