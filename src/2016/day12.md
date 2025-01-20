# 12日目：レオナルドのモノレール

ついにこの建物の最上階に到達しました：
傾斜したガラス天井のある庭です。
これ以上取るべきスターはないようです。

**オニユリ**に囲まれた近くのベンチに座りながら、
階下のサーバーから抽出したファイルの一部を何とかして解読しました。

これらの文書によると、イースターバニーHQはこの建物だけでなく、この近辺のいくつかの建物からなるようです。
それらはすべて局所的な(or 各駅停車の)モノレールで接続されており、
ここからそう遠くない所に別の建物があります！
残念ながら、夜なのでモノレールは現在動作していません。

リモートでモノレール制御システムに接続し、起動シーケンスにパスワードが必要であることを発見しました。
パスワードチェックロジック（パズル入力）は簡単に抜き取れましたが、使用するコードは奇妙です。
これは、組み立てたばかりの新しい[**コンピュータ**](./day11.md)用に設計されたアセンバニーコードです。
コードを実行してパスワードを取得する必要があります。

あなたが抜き取ったアセンバニーコードは4つのレジスタ (a, b, c, d) を操作します。
レジスタの初期値は0で、任意の整数を保持できます。
ただし、いくつかの**命令**のみを使用するようです：

- `cpy x y` ： `x`（整数またはレジスタの**値**）をレジスタ`y`に**コピー**します。
- `inc x` ： レジスタ`x`の値を**1増やし**ます。
- `dec x` ： レジスタ`x`の値を**1減らし**ます。
- `jnz x y` ：`x`が**零でない**場合のみ、`y`離れた（正は前方を意味し、負は後方を意味します）命令に**分岐**します。

`jnz`命令は、それ自体に対して相対的に移動します：
オフセット`-1`は直前の命令に継続し、
オフセット2は次の命令を**飛ばし**ます。

例えば：

```
cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a
```

上記のコードは、レジスタaを41に設定し、その値を2増加させ、
その値を1減少させ、そして最後の`dec a`を飛ばして
（aは零ではないので、`jnz a 2`は次の命令を飛ばす）
レジスタaに42を残します。
最後の命令を通過すると、プログラムは停止します。

パズル入力のアセンバニーコードを実行した後、**レジスタaにはどのような値が残っていますか？**

# パート2 #

モノレールの非常階段に向かうと、モノレールが起動しなかったことがわかりました。
レジスタ`c`は起動キーの位置に初期化する必要があります。

**レジスタ`c`を0でなく1に初期化する**と、今回はどのような値がレジスタ`a`に残りますか？