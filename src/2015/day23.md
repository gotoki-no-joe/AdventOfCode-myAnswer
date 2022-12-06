# 23日目：チューリングロックを開く

Little Jane Marieはとある篤志家からクリスマスに彼女の最初のコンピュータを贈られました。
説明書とサンプルプログラムが付属していますが、コンピュータ自体が誤動作しているようです。
彼女はそのプログラムが何をするのか知りたがっているので、
彼女がそれを実行するのをあなたに手伝ってほしいのです。

このマニュアルでは、
コンピュータが2つのレジスタと6つの命令をサポートしていると説明しています。
（本当に、読者に確認しておくと、これは最新技術です。）
レジスタには`a`と`b`という名前が付けられ、負でない整数を保持できます。
値が0の状態で始まります。
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

<details><summary>解説</summary><div>

命令を観察すると、指定したレジスタの内容を演算して更新するものと、
レジスタの内容に関する条件によってジャンプをするもののふたつに分かれていることがわかる。
（無条件ジャンプも「常に成立する」条件が指定されていると見なせる。）

後で、レジスタの実体は配列で実現すると想定して、レジスタは番号で呼ぶことにする。
これらを踏まえて、命令を表す代数的データ型を定義する。

```haskell
type Reg = Int
data Inst = Ialu (Int -> Int) Reg | Ijmp (Int -> Bool) Reg Int
```

命令一行を読み込む関数を定義する。
このとき命令に応じて妥当な関数を設定する。

```haskell
parse :: String -> Inst
parse xs =
  case words xs of
    ["hlf", reg] -> Ialu (flip div 2) (regp reg)
    ["tpl", reg] -> Ialu (3 *)        (regp reg)
    ["inc", reg] -> Ialu succ         (regp reg)
    ["jmp", ofs] -> Ijmp (const True) undefined (readofs ofs) -- 無条件
    ["jie", reg, ofs] -> Ijmp even   (regp reg) (readofs ofs)
    ["jio", reg, ofs] -> Ijmp (1 ==) (regp reg) (readofs ofs)
  where
    regp ('a':_) = 0             -- 後ろのコンマを無視できるように
    regp ('b':_) = 1
    readofs ('+':cs) = read cs   -- 数値は'+'付きだとreadできない
    readofs cs = read cs
```

プログラムカウンタが逸脱するまで命令を実行し続けるCPUエミュレータを作る。

```haskell
exec :: Array Int Inst -> Int -> Array Reg Int -> Array Reg Int
exec prog pc regF
  | not $ inRange (bounds prog) pc = regF
  | otherwise =
    case prog ! pc of
      Ialu f r -> exec prog (succ pc) (regF // [(r, f $ regF ! r)])
      Ijmp p r ofs | p (regF ! r) -> exec prog (pc + ofs) regF
                   | otherwise    -> exec prog (succ pc)  regF
```

入力を読み込み、レジスタを0に初期化して実行し、レジスタの最終状態を観察する。

```haskell
main = do
  co <- readFile "input.txt"
  let is = map parse $ lines co
  let prog = listArray (1, length is) is
  putStrLn "part 1"
  print $ exec prog 1 (listArray (0,1) [0,0])
```

</div></details>

# パート2

匿名の篤志家はあなたがまんまと騙されて結果を出してくれたこと、ではなくて、
コンピュータのことでLittle Jane Marieを助けてくれたことを**とても**感謝しています。
レジスタaが代わりに1で始まった場合、
プログラムが実行を終了したときのレジスタbの値は何ですか？
という問題は、決してあなたを失望させることはないでしょう。
（ちょっと英語がよくわからない。）

<details><summary>解説</summary><div>

レジスタの初期値を変えて実行するだけ。

```haskell
main = do
  ...
  putStrLn "part 2"
  print $ exec prog 1 (listArray (0,1) [1,0])
```

</div></details>
