# 入力

カード番号はいらないと信じて、コロンまで全て捨てる。
当選番号と自分の番号を分けるために、縦線文字で区切る。
それぞれwordsでバラして整数のリストふたつにする。

```haskell
import Data.List

parse :: String -> ([Int], [Int])
parse l = (map read $ words l2, map read $ words l3)
  where
    _:l1 = dropWhile (':' /=) l
    (l2,_:l3) = break ('|' ==) l1

runner i f = do
  cards <- map parse . lines <$> readFile i
  print $ f cards

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 cards = ...
```

# パート1

言われるとおりに計算する。

```haskell
part1 cards = sum $ map score cards

score (xs, ys)
  | cnt == 0  = 0
  | otherwise = 2 ^ pred cnt
  where
    cnt = length $ filter (flip elem xs) ys
```

# パート2

## カードの番号は必要だった？！

`input.txt` のカード番号が行番号と等しいかどうか、急いで確認する。

```haskell
ghci> readFile "input.txt" >>= print . and . zipWith (==) [1 ..] . map (read . take 3. drop 5) . lines
True
```

あぁ助かった。

## 本題

カードの取得をシミュレーションすると、大変な事になりそうだ。
有限の枚数で答えが出る問題なはずなので、貰えるカードの関係にループはないはず。
なので、カードそれぞれについて、カード $k$ 1枚が、
それ自身を含めて何枚のカードになるかをもつ表 $N[k]$ を作る。

カード番号 $k$ に当たりが $m$ 個あるとき、
$$N[k] = 1 + N[k+1] + \dots + N[k + m]$$
もっとちゃんと書くと、カードの番号の最大値を$U$として、
$$N[k] = 1 + \sum_{i=k+1}^{\min U (k+m)} N[i]$$

正格評価の言語の場合、$N[U]$ から $N[1]$ へ逆順に計算すれば支障なく求められる。
遅延評価なHaskellでは、この定義をそのまま書ける。

求める答えは $\displaystyle \sum_{i=1}^U N[i]$ である。

```haskell
import Data.Array

part2 cards = sum $ elems arr
  where
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    u = length cards
    arr = listArray (1, u)
      [ succ $ sum $ map (arr !) [succ i .. min u $ i + m]
      | (i, m) <- zip [1 ..] wins ]
```

正格言語の方式を模倣した版

```haskell
part2a cards = sum nsN
  where
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    nsN = foldr step [] wins
    step w ns = (1 + sum (take w ns)) : ns
```

## 別解

クジは、必ず自分の後ろの番号のものが貰える。
なので、クジを前から順に考えて、現在注目している番号のクジの枚数が、その後ろ数枚のクジがそれぞれ貰える枚数になる。
クジの当たりの数だけ、後ろのクジにそれらを配る。

```haskell
part2b cards = sum ns
  where
    (_,ns) = mapAccumL step ones wins
    ones = map (const 1) cards -- 持ちクジ数初期値
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    step (n:ns) m = (ns3, n)
      where
        (ns1, ns2) = splitAt m ns
        ns3 = map (n +) ns1 ++ ns2
```

これはかなり命令的な解なので、そう書いた方がわかりやすいか。

```haskell
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

part2c cards = runST $
  do
    ns <- newArray (1,u) 1 :: ST s (STUArray s Int Int) -- 持ちクジ数配列、初期値1
    forM_ (zip [1 ..] wins) (\(i, m) -> do       -- クジ1から順に、1枚あたりの当たり数mのクジiが
      k <- readArray ns i                        -- k枚あるから
      forM_ [succ i .. min u $ i + m] (\j -> do  -- クジ i+1～i+m (上限u) を
        t <- readArray ns j
        writeArray ns j $ t + k                  -- k枚ずつ増やす
        )
      )
    sum <$> getElems ns
  where
    u = length cards
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards]
```
