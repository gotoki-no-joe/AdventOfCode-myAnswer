# 入力

行ごとの文字列として渡す。

```haskell
runner i f = readFile i >>= print . f . lines
```

# パート1

文字を前から食べていく、オートマトン的なものを作る。

- ABBAを発見したとき、角括弧の中にいるかどうかで、どちらを発見したかを記憶する。
- 角括弧の中にいるかどうかを記憶する。
- 最後に、括弧の外で発見し、中で発見しなかったら成功

とするために、状態は

- 括弧の外でABBAを検出したことがあるかどうか (`f1`)
- 括弧の中でABBAを検出したことがあるかどうか (`f2`)
- 現在地は括弧の入れ子の何段目か (`d`)

を持つ。

```haskell
isABBA :: String -> Bool
isABBA = loop False False 0
  where
    loop f1 f2 _ [] = f1 && not f2
    loop f1 f2 d ('[':xs) = loop f1 f2 (succ d) xs
    loop f1 f2 d (']':xs) = loop f1 f2 (pred d) xs
    loop f1 f2 d (x:y:z:w:xs) | x == w, y == z, x /= y = if d == 0 then loop True f2 d xs else loop f1 True d xs
    loop f1 f2 d (_:xs) = loop f1 f2 d xs
```

疑い深いので、

- 括弧の閉じが足りていないということがないか
- 括弧が入れ子になっているか（これは許容するプログラムになっているが）
- 閉じ括弧が多すぎることもないか

を検査するコードも入れる。

```haskell
    loop _ _ d [] | d > 0 = error "bracket open left"
    loop _ _ d ('[':_) | d > 0 = error "nesting bracket"
    loop _ _ d (']':_) | d < 1 = error "too many closing bracket"
```

あとはこれを全ての行に試すだけ。

```haskell
part1 = length . filter isABBA

main1 = runner part1
```

# パート2

大きな流れはパート1と似ているが、その場で判断を下せず、
全てのABAとBABを集めておいて、対応するものがあるかを判定する段階を踏む。
そのためまずは、ABA に対して (A,B) を、BAB に対して別の入れ物にこちらも (A,B) を入れる。

パート1で括弧は入れ子にならないと確認できたため、括弧の深さでなく括弧内かどうかだけ記憶している。

入力が終わったら判定する。

```haskell
import Data.List

isSSL = loop [] [] False
  where
    loop abs bas _ [] = not $ null $ intersect abs bas
    loop abs bas f  ('[':xs) = loop abs bas True  xs
    loop abs bas f  (']':xs) = loop abs bas False xs
    loop abs bas f (x:xs@(y:z:_)) | x == z, x /= y = if f then loop abs ((y,x):bas) f xs else loop ((x,y):abs) bas f xs
    loop abs bas f (_:xs) = loop abs bas f xs

part2 = length . filter isSSL

main2 = runner part2
```
