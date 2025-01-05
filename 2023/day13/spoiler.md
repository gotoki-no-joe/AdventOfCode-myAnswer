# 入力

空行で区切ってパターンのリストとして渡す。

```haskell
import Data.List.Split

runner i f = do
  lss <- wordsBy null . lines <$> readFile i
  print $ f lss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 lss = ...
```

# パート1

リストでお手玉しながら、両方があるだけ等しい位置を見つければよい。
縦横を入れ替えるには `transpose` を使う。
解が見つからなかったとき `Nothing` を返す。

```haskell
import Data.List
import Control.Applicative
import Data.Maybe

part1 lss = sum $ map score lss
  where
    score ls = fromJust $ (100 *) <$> sub ls <|> sub (transpose ls)
    sub ls = uncurry (loop 1) $ splitAt 1 ls
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | and $ zipWith (==) as bbs = Just k
      | otherwise = loop (succ k) (b : as) bs
```

パターンの縦横が64を越えない程度のようなので、二進数に変換しておくと比較が高速にできる。
そこまでする必要もなさそうだが、違いは `sub` の行だけで他は全く変わらない。

```haskell
import Data.Bits

l2b :: String -> Int
l2b "" = 0
l2b ('.':cs) = l2b cs .<<. 1
l2b ('#':cs) = l2b cs .<<. 1 .|. 1

part1a lss = ...
  where
    ...
    sub ls = uncurry (loop 1) $ splitAt 1 $ map l2b ls
    ...
```

# パート2

判定条件が「全て完全に一致」から「違う点が一箇所だけ」に変更になった。
「全て完全に一致」を計算していたのは `and $ zipWith (==) as bbs` である。

リストで「一箇所だけ異なる」をするには、文字単位で異なる箇所の個数を数えた結果を行ごとに合算する。

```haskell
diff0 as bs = and $ zipWith (==) as bs
diff1 as bs = 1 == sum (zipWith diffCount as bs)
  where
    diffCount xs ys = length $ filter id $ zipWith (/=) xs ys
```

ビットでするには、`xor` をとった結果の `popCount` が、行における異なる位置の個数になる。

```haskell
diff1a as bs = 1 == sum (zipWith diffCount as bs)
  where
    diffCount x y = popCount $ xor x y
```

`diff0`, `diff1` を差し替えられるように `part1` を改変する。
ついでに `l2b` を挟むかどうかも変更可能にする。

```haskell
part12 pre prop lss = sum $ map score lss
  where
    score ls = fromJust $ (100 *) <$> sub ls <|> sub (transpose ls)
    sub ls = uncurry (loop 1) $ splitAt 1 $ map pre ls  -- pre ココ
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | prop as bbs = Just k                            -- prop ココ
      | otherwise   = loop (succ k) (b : as) bs

test1a = runner "sample.txt" $ part12 id diff0
main1a = runner "input.txt"  $ part12 id diff0
main1b = runner "input.txt"  $ part12 l2b diff0

test2a = runner "sample.txt" $ part12 id diff1
main2a = runner "input.txt"  $ part12 id diff1
main2b = runner "input.txt"  $ part12 l2b diff1a
```
