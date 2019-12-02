# 23日目：チューリングロックを開く #

リトルジェーンマリーはとある篤志家から
クリスマスに彼女の最初のコンピュータを贈られました。
説明書とサンプルプログラムが付属していますが、
コンピュータ自体が誤動作しているようです。
彼女はそのプログラムが何をするのか知りたがっているので、
彼女がそれを実行するのをあなたに手伝ってほしいのです。

このマニュアルでは、
コンピュータが2つのレジスタと6つの命令をサポートしていると説明しています。
（本当に、読者に確認しておくと、これは最新技術です。）
レジスタには`a`と`b`という名前が付けられ、負でない整数を保持できます。
値は0で始まります。
命令は次のとおりです。

- `hlf r` レジスタrを現在の値の**半分に**設定してから、次の命令に進みます。
- `tpl r` レジスタrを現在の値の**3倍に**設定してから、次の命令に進みます。
- `inc r` レジスタrを**インクリメント**する、すなわちそれに1を足して、次の命令に進みます。
- `jmp offset` は**ジャンプ**です。
それはそれ自身に対して相対的に`offset`離れた命令に進みます。
- `jie r, offset` は`jmp`に似ていますが、
レジスタrが偶数の場合にのみジャンプします。
（jump if even 「偶数の場合はジャンプ」）
- `jio r, offset` は`jmp`に似ていますが、
レジスタrが1の場合にのみジャンプします。
（jump if one 「1であればジャンプ」「奇数」ではないので注意。）

3つのジャンプ命令はすべて、その命令に対する相対的**オフセット**で機能します。
オフセットは常にジャンプの方向を示す接頭辞`+`または`-`を付けて
（それぞれ順方向または逆方向）書かれます。
例えば、`jmp +1`は単に次の命令に進むだけで、
`jmp +0`は継続的にそれ自体に永遠に戻ります。

プログラムは、定義されている命令を超えて命令を実行しようとすると終了します。

たとえば、このプログラムは、`a`を`2`に設定します。
それは`jio`命令によって`tpl`命令がスキップされるためです。

~~~
inc a
jio a, +2
tpl a
inc a
~~~

あなたのパズル入力のプログラムが実行を終了したときの**レジスタbの値**は何ですか？

# パート2 #

匿名の篤志家は(releasi.. ??)に**非常に**感謝しています。
彼女のコンピュータに関してリトルジェーンマリーを助けたことに。
間違いなくあなたの気を散らさないために(?)
レジスタaが代わりに1で始まった場合、
プログラムが実行を終了したときのレジスタbの値は何ですか？