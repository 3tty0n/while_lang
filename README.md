# while_lang

A small language that is compiled to Python bytecode. This language is designed for teaching compiler construction for bachelor students.

## Prerequisite

- OCaml >= 4.14
- `wabt`

### Install OCaml

#### MacOS

```shell
$ brew install ocaml
$ brew install wabt
```

#### Windows

Plese use WSL or WSL2! And hit `sudo apt install ocaml wabt`.

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

このコンパイラは、While 言語という繰り返し構造を持つ言語のソースコードから
WebAssembly 中間表現 (.wat) へコンパイルします。コンパイルされた .wat はブラウザ
上で実行できます。[専用のアプリケーション](http://www.yuiza.org/wonline)を活用してください。

ファイルの構造は以下の通りです。

- `syntax.ml`: 構文木 (文法) を定義する
- `parser.mly`: 構文解析のルールを定義する。パーサの生成にはocamlyaccを用いる
- `lexer.mll`: 字句解析のルールを定義する。レキサの生成にはocamllexを用いる
- `virtual_stack.ml`: 仮想スタックマシンの命令セット、命令セットへのコンパイラが
  含まれる
- `emit_wasm.ml`: WebAssembly 中間表現への変換ルールを定義する。

コンパイルの流れは以下の通りです。

```
 (while 言語) --- lexing (lexer.mll) --- parsing (parser.mly) --> (構文木、syntax.ml)
             --- virtual_stack.ml    --> (仮想スタックマシン命令列)
             --- emit_wasm.ml        --> (WebAssembly 中間表現)
```

## 開発環境

本コンパイラの実装には OCaml を用います。OCaml は macOS では Homebrew, Windows では cygwin を用いたイ
ンストーラが提供されています。

### macOS

```
$ brew install ocaml
```

### Windows

Windows Subsystem Linux 2 (WSL2) の使用を推奨します。その他、 Ocaml for Windows
という cygwin 拡張があります。

#### OCaml for Windows

[OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/installation/) から
`64-bit` をクリックして GUI インストーラを使用してインストールしてください。
cygwin カスタム環境が構築され、cygwin で OCaml が使えるようになります。

### エディタ

エディタは各自好きなものを使用してください。オススメは Visual Studio Code です。開発支援ツールが充実しています。

## 開発の仕方

- コンパイルする

    ```shell
    $ make
    ```

- While 言語を `.wat` へコンパイルする

    ```
    $ ./while_lang test/assign.while
    ```

- `.wat` の実行結果を確認する

  - [専用アプリケーション](https://www.yuiza.org/wonline)を開き、 `.wat` を読み込ませる。


- (その他) `.wat` から `.wasm` へコンパイルする
    - 別途 `wabt` のインストールが必要です

    ```
    $ wat2wasm test/assign.wat -o test/assign.wasm
    ```


- (その他) 中間コードなどを消す

    ```shell
    $ make clean
    ```
