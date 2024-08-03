# while_lang

A small language that is compiled to Python bytecode. This language is designed for teaching compiler construction for bachelor students.

## Prerequisite

- OCaml >= 4.14
- Python 2.7 (not Python 3! sorry)

### Install OCaml

#### MacOS

```shell
$ brew install ocaml
```

#### Windows

Plese use WSL or WSL2! And hit `sudo apt install ocaml`.

For cygwin users: see this document (https://fdopen.github.io/opam-repository-mingw/installation/)

## Usage

### Build

```shell
$ make
```

### Test

```shell
$ make test
```

### Clean-up


```shell
$ make clean
```

---

(for Japanese)

# while_lang

このコンパイラは、While 言語という繰り返し構造を持つ言語のソースコードから Python 2 (not
3!) のバイトコードへコンパイルします。実験・教育用途で制作しました。

ファイルの構造は以下の通りです。

- `syntax.ml`: 構文木 (文法) を定義する
- `parser.mly`: 構文解析のルールを定義する。パーサの生成にはocamlyaccを用いる
- `lexer.mll`: 字句解析のルールを定義する。レキサの生成にはocamllexを用いる
- `virtual_stack.ml`: 仮想スタックマシンの命令セット、命令セットへのコンパイラが含まれる
- `emit_pyc.ml`: Python のバイトコードへ変換するためのコンパイラが含まれる
- `assemble_pyc.ml`: While 言語コンパイラでコンパイルされたバイトコードを Python オブジェ
    クトへ直列化するためのコンパイラ。Python インタプリタでコンパイルコードを実行するために必要。

コンパイルの流れは以下の通りです。

```
 (while 言語) --- lexing (lexer.mll) --- parsing (parser.mly) --> (構文木、syntax.ml)
             --- virtual_stack.ml   --> (仮想スタックマシン命令列)
             --- emit_pyc.ml        --> (Pythonバイトコード命令列)
             --- assemble_pyc.ml    --> (Pythonバイトコードオブジェクト)
```

## 開発環境

本コンパイラの実装には OCaml を用います。OCaml は macOS では Homebrew, Windows では cygwin を用いたイ
ンストーラが提供されています。

### macOS

```
$ brew install ocaml
```

### Windows

[OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/installation/) から
`64-bit` をクリックして GUI インストーラを使用してインストールしてください。
cygwin カスタム環境が構築され、cygwin で OCaml が使えるようになります。

また、エディタは各自好きなものを使用してください。オススメは Visual Studio Code です。開発支援ツールが充実しています。

## 開発の仕方

コンパイルする

```shell
$ make
```

中間コードなどを消す

```shell
$ make clean
```

While 言語を `.pyc` へコンパイルする

```shell
$ ./while_lang test/assign.while
```

`.pyc` を Python2 インタプリタで実行する

```shell
$ ./interpret.py test/assign.pyc
```
