# パート1

繰り上がりありで列をインクリメントするには、逆順になっているとHaskell的には都合がよい。

```haskell
incr ('z':cs) = 'a' : incr cs  -- 繰り上がり
incr ('h':cs) = 'k' : cs       -- 禁止文字iは飛ばす
incr ('n':cs) = 'p' : cs       -- 禁止文字oは飛ばす
incr ('k':cs) = 'm' : cs       -- 禁止文字lは飛ばす
incr ( c :cs) = succ c : cs
incr "" = ""
```

要件1を判定する。
インクリメント列は、後ろからはデクリメント列に見える。

```haskell
cond1 cs@(c1:c2:c3:_) = (succ c3 == c2 && succ c2 == c1) || cond1 (tail cs)
cond1 _ = False
```

要件2は、上の `incr` の結果では生成はしないが、上位桁に最初からあるものを除去できない。
しかし、パズル入力にそれがなければ、これ以上気にする必要はない。

要件3は、連続する2文字が等しいかどうかの列を考え、
一つ`True`を見つけたとき、その直後は `aaa` のような並びなので捨てて、
それより後ろにもう一つ`True`があればよい。

```haskell
cond3 cs =
  case dropWhile not $ zipWith (==) cs $ tail cs of
    True:bs -> or $ drop 1 bs
    _       -> False
```

全体をまとめる。

```haskell
next :: String -> String
next = reverse . until cond13 incr . incr . reverse

cond13 xs = cond1 xs && cond3 xs

part1 = next "(パズル入力)"
```

# パート2

パート1の結果をもう一度処理するだけ。

```haskell
part2 = next part1
```

こちらは先ほどより時間がかかる。
が、待つほかない。

# おや？

ここまで書いて、要件2の解釈がおかしいかもしれないことに気がついた。
文字 `i`,`o`,`l` を含めることができない、という制約の下で、
`ah...` をインクリメントして、くり上がりにより `h` を `i` にしようとして、
禁止文字なので飛ばして `j` にすると、下の桁 `...` はゼロリセットされる必要があるのでは？

といって、くり上がりにより`h`がインクリメントの対象になったのだから、
下の桁 `...` は実際には既に `aaa` とゼロリセットされているので問題なかった。
どっとはらい。
