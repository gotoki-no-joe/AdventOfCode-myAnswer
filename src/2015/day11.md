# 11日目：企業方針

サンタの以前のパスワードは期限切れです。
新しいパスワードを選択するのに手助けが必要です。

古いパスワードが期限切れになった後に
新しいパスワードを覚えるのを助けるために、
サンタは前のパスワードに基づいてパスワードを設定する方法を考案しました。
企業の方針では、
セキュリティ上の理由からパスワードは全て小文字で
ちょうど8文字にする必要があるため、
古いパスワード文字列を有効になるまで
繰り返し**インクリメント**して新しいパスワードを探します。

インクリメントはちょうど数字で数えるようなものです。
`xx`, `xy`, `xz`, `ya`, `yb`, などと続きます。
一番右の文字を1つ次にしてください。
それが`z`であった場合は`a`に巻き戻り、
巻き戻りしなくなるまで左の次の文字について繰り返します。

サンタにとって残念なことに、新しいセキュリティ担当の小人が最近着任しました。
彼はいくつかの追加パスワード要求を課しています。

- パスワードは少なくとも3文字の連続したインクリメントした並びを一つ含む必要がある。
それは`abc`, `bcd`, `cde` から `xyz` までのようなものである。
文字をスキップすることはできない。`abd`はこれに含まれない。
- パスワードには文字`i`, `o`, `l` を含めることはできない。
これらの文字は他の文字と間違える可能性があり、
したがって混乱する可能性があるためである。
- パスワードは`aa`, `bb`, `zz`のような文字の対を、重なりなく別個に少なくとも2つ含む必要がある。

例えば：

- `hijklmmn` 第1の要件を満たしています。（インクリメント列`hij`が含まれています。）
しかし第2の要件に失敗しています。（`i`および`l`が含まれています。）
- `abbceffg` 第3の要件を満たしています。（`bb`と`ff`を繰り返しています。）
しかし第1の要件を満たしていません。
- `abbcegjk` 第3の要件に失敗します。なぜなら、二重文字がひとつ(`bb`)しかないからです。
- `abcdefgh`のパスワードは`abcdffaa`です。
- `ghijklmn`のパスワードは`ghjaabcc`です。
これは`i`が許されないために`ghi...`で始まるすべてのパスワードを結局スキップするためです。

サンタの現在のパスワード（あなたのパズル入力）が与えられたとき、
彼の**次のパスワード**は何ですかか？

あなたのパズル入力は`hxbxwxba`です。

<details><summary>解説</summary><div>

繰り上がりありで列をインクリメントするには、逆順になっているとHaskell的には都合がよい。

```haskell
incr ('z':cs) = 'a' : incr cs  -- 繰り上がり
incr ( c :cs) = succ c : cs
  | elem c "iol" = succ (succ c) : cs -- 禁止文字は飛ばす
  | otherwise    = succ       c  : cs
incr "" = ""
```

インクリメント列は、後ろからはデクリメント列に見える。

```haskell
cond1 (c1:c2:c3:_) | succ c3 == c2 && succ c2 == c1 = True
cond1 (_:cs) = cond1 cs
cond1 [] = False
```

禁止文字はインクリメントでは出現しないが、
初期文字の全ての禁止文字がインクリメントで消えるまで捨て続けるのも無駄になる。
禁止文字が全て消えた最初の文字列とは、元の順序で最も前にある禁止文字を次の文字にし、
それ以降を全て `a` にしたものである。これを構築することで条件2の判定に代える。

```haskell
clearCond2 "" = ""
clearCond2 (c:cs)
  | elem c "iol" = succ c : map (const 'a') cs
  | otherwise    = c : clearCond2 cs
```

最後の例が `ghjaabaa` とならず `ghjaabcc` であることから、条件3の「別個」が厳しい意味であるとわかる？
そうでなくて、"abc"という続きが必要だ、という条件の方らしい。厳しくないのなら `nub` を消せばよい。

```haskell
cond3 cs =
  case nub [c | (c,d) <- zip cs $ tail cs, c == d] of
    (_:_:_) -> True
    _       -> False
```

全体をまとめる。

```haskell
part1 :: String -> String
part1 = reverse . until cond13 incr . reverse . clearCond2

cond13 xs = cond1 xs && cond3 xs
```

</div></details>

# パート2

サンタのパスワードが再び期限切れになりました。次は何ですか？

<details><summary>解説</summary><div>

パート1の結果を `part1` にかけるだけで、コードの追加はない。

</div></details>