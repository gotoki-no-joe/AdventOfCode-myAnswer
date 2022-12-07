# 1日目：タクシーの時間がない

あなたはどこかの都市の**イースターバニー司令部**の近くへ空中投下されました。
「近く」とは残念ながらできるだけ近くした結果でしかありません。
サンタ付きの小人たちが入手した
イースターバニー募集要項の指示はこの地点から開始されており、
それ以上にこれを分析する時間はありませんでした。

要項は、
あなたが与えられた座標（あなたが着地した場所）から、
北に向いた状態で始めるよう指示しています。
次に、90度左（L）または右（R）のいずれかの方向に向きを変えて、
指定した数のブロックを進み、新しい交差点で終了します。

このようなばかげた指示に徒歩で従う時間はありませんので、
少し時間を割いて目的地を見つけましょう。
**碁盤の目**のようになった都市の**道だけを歩いて**、
目的地までの最短距離はどれだけですか？

## 例 ##

- `R2, L3` に従うと2ブロック東3ブロック北に到着し、
それは出発位置から5ブロック離れています。
- `R2, R2, R2` に従うと2ブロック南に到着し、
それは2ブロック離れています。
- `R5, L5, R5, R3` は12ブロック離れた場所に到着します。

イースターバニー司令部は**何ブロック先**にありますか？

<details><summary>解説</summary><div>

まずは、入力を読み込んで、左右と進むブロック数のコマンド列に変換する。

```haskell
import Data.Char

main1 = do
  co <- readFile "input.txt"
  let is = parse co
  print is

parse :: String -> [Either Int Int]
parse = loop
  where
    loop ('L':xs) = sub Left  xs
    loop ('R':xs) = sub Right xs
    loop "" = []
    sub f xs = f (read as) : loop (dropWhile (not.isUpper) bs)
      where
        (as,bs) = span isDigit xs
```

現在位置 \\((x,y)\\), 現在の向き \\((d_x, d_y)\\) を状態にして、コマンド列を消化する。

```haskell
step ((x,y), (dx,dy)) (Left  n) = ((x - n * dy, y + n * dx), (- dy, dx))
step ((x,y), (dx,dy)) (Right n) = ((x + n * dy, y - n * dx), (dy, - dx))

main1 = do
  ...
  let ((x,y),_) = foldl step ((0,0), (0,1)) is
  print $ abs x + abs y
```

</div></details>

# パート2

その後、募集要項の裏面に指示が続いていることに気がつきました。
本当のイースターバニー司令部はあなたが最初に2度訪れる場所にあります。

たとえば、指示が R8, R4, R4, R8 である場合、
最初に2回訪れた場所は東に4ブロック離れた所です。

あなたが**最初に2度訪れる場所**は何ブロック離れていますか？

<details><summary>解説</summary><div>

細かくステップを分けて考える。
コマンド列から、1マスずつの差分の列を生成し、
原点上向きから出発して全ての座標の列に直し、
先頭から消費し、既出の座標を発見したところで停止する。

```haskell
import Data.Set
import qualified Data.Set as S
import Data.List

deltas :: [Either Int Int] -> [(Int, Int)]
deltas = concat . snd . mapAccumL step (0,1)
  where
    step (dx,dy) (Left  n) = sub n (- dy, dx)
    step (dx,dy) (Right n) = sub n (dy, - dx)
    sub n dxy = (dxy, replicate n dxy)

positions is = scanl add (0,0)
  where
    add (x, y) (dx, dy) = (x + dx, y + dy)

loop s (p:ps)
  | S.member p s = p
  | otherwise    = loop (S.insert p s) ps

main1 = do
  ...
  let (z,w) = loop S.empty $ positions $ deltas1 is
  print $ abs z + abs w
```

</div></details>
