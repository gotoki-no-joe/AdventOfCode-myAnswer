# 4日目:セレス検索

「主任はここにいないようです。次いってみよう！」
歴史学者の1人がデバイスを取り出し、その唯一のボタンを押します。
一瞬の閃光の後、あなたは[セレス監視ステーション](../2019/day10.md)の中にいることに気づきました！

主任の捜索が続く中、監視ステーションに駐在している小さな小人があなたのシャツを引っ張っています。
彼女は、あなたが彼女の[シークワーズ](https://ja.wikipedia.org/wiki/%E3%82%B7%E3%83%BC%E3%82%AF%E3%83%AF%E3%83%BC%E3%82%BA)（あなたのパズル入力）を手伝ってくれるかどうか知りたがっています。
彼女は単語 `XMAS` それだけを見つける必要があります。

このシークワーズでは、単語を水平、垂直、斜め、逆方向、他の単語と重なるものも許します。
少しだけ普通でないのは、`XMAS`を一つだけ見つけるのではなく、それを全て見つける必要があります。
無関係な文字を `.` で置き換えて、`XMAS` が現れる形のいくつかを示します：

```
..X...
.SAMX.
.A..A.
XMAS.S
.X....
```

実際のシークワーズは、文字でいっぱいです。
例：

```
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
```

このシークワーズには `XMAS` が合計18個出現しています。
同じシークワーズの、`XMAS` に含まれていない文字を `.` に置き換たものを示します：

```
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
```

小さな小人のシークワーズを見てみましょう。`XMAS` は何個出現していますか？

# パート2

小人はあなたを不思議そうに見ています。やることを勘違いしていたかも？

シークワーズをひっくり返して説明書きを探すと、これは実際には `XMAS` パズルではないことがわかりました。
これは `X-MAS` パズルです。ふたつの `MAS` が `X` の形になっているものを見つけます。
それを達成する1つの方法は次のようなものです：

```
M.S
.A.
M.S
```

上の図では、無関係な文字が再び `.` に置き換えられています。
`X` の中では、X内では、各 `MAS` は順方向または逆方向に書かれてもよいです。
これは以前と同じ例ですが、今回は全ての `X-MAS` が残してあります：

```
.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
```

この例では、`X-MAS` が9個出現しています。

説明書きから裏返してシークワーズに戻して、やり直してください。
`X-MAS` は何個出現していますか？