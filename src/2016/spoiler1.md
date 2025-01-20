# 入力

書式は、コンマ区切りで、`R`または`L`の1文字につづい歩数の数字列の並び。
それ用では全くないが、`[Either Int Int]` を使うとちょうど表せるのでそうする。
（実際には1ビットあればよいので、符号を流用するだけで足りるのだが。）

空白も入っているので、コンマは取り除いてしまえばよいだろう。

```haskell
parse :: String -> [Either Int Int]
parse = map p . words . filter (',' /=)
  where
    p ('L':ds) = Left  $ read ds
    p ('R':ds) = Right $ read ds

runner f = readFile "input.txt" >>= print . f . parse
```

# パート1

現在位置 \\((x,y)\\), 現在の向き \\((d_x, d_y)\\) を状態にして、コマンド列を消化する。

```haskell
part1 cs = abs x + abs y
  where
    ((x,y),_) = foldl step ((0,0), (0,1)) cs
    step ((x,y), (dx,dy)) (Left  n) = ((x - n * dy, y + n * dx), (- dy, dx))
    step ((x,y), (dx,dy)) (Right n) = ((x + n * dy, y - n * dx), (dy, - dx))

main1 = runner part1
```

# パート2

パート1では、指示に対して移動後の絶対座標を計算していたが、
代わりに、初期状態北向きから始めて、1マスずつの差分の列だけを生成する。
これで、状態は現在の向きだけを持ち回せばよくなる。

次に、原点から開始し、差分を累積することで座標の列にする。

この座標列を前から消費し、既出の座標を発見するまで続ける。

```haskell
import qualified Data.Set as S
import Data.List

part2 cs = abs x + abs y
  where
    (_,dss) = mapAccumL step (0,1) cs
    step (dx,dy) (Left  n) = sub n (- dy, dx)
    step (dx,dy) (Right n) = sub n (dy, - dx)
    sub n dxy = (dxy, replicate n dxy)

    positions = scanl add (0,0) $ concat dss
    add (x, y) (dx, dy) = (x + dx, y + dy)

    (x,y) = loop S.empty positions
    loop s (p:ps)
      | S.member p s = p
      | otherwise    = loop (S.insert p s) ps

-- どうしてもループしたくないときの別解
    (x,y) = fst $ head $ filter (uncurry S.member) $
            zip positions $ scanl (flip S.insert) S.empty positions
```
