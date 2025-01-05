# 入力

コンマ区切りをバラした文字列のリストで渡す。

```haskell
import Data.List.Split

runner i f = do
  steps <- wordsBy (',' ==) . head . lines <$> readFile i
  print $ f steps

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 steps =
```

# パート1

定義通りに計算する。

```haskell
import Data.Char
import Data.List

hash = foldl' step 0
  where
    step acc c = mod ((acc + ord c) * 17) 256

part1 = sum . map hash
```

# パート2

それぞれのステップについて、末尾が `-` のとき、それを除くとラベルとなる。
そうでないときは数字で、これが焦点距離、その前の `=` も除くとラベルとなる。

初期化シーケンスは mutable array が前提のような内容だが、
実行途中の配列の値が後の計算に影響することはないので、実は `accumArray` で対応できる。
添え字は箱の番号0から255になり、内容はラベルと焦点距離の対のリストとする。

`accumArray` のための演算には `flip ($)` を指定し、レンズリストに対して計算をする関数を値として投入する。

```haskell
import Data.Array

-- 指定したラベルのレンズを除く
delLens label ls = filter ((label /=) . fst) ls

-- 指定したラベルのレンズがあるとき、焦点距離を指定のものに差し替える。
-- ないとき、末尾に挿入する
insLens label focus [] = [(label, focus)]
insLens label focus (l:ls)
  | fst l == label = (label, focus) : ls
  | otherwise = l : insLens label focus ls

part2 steps = ...
  where
    arr = accumArray (flip ($)) [] (0, 255) $
          map makeCmd steps
    makeCmd s
      | last s == '-' = (hash lab1, delLens lab1)
      | otherwise     = (hash lab2, insLens lab2 (digitToInt $ last s))
      where
        lab1 = init s
        lab2 = init lab1
```

あとは、箱の中身を取り出して答えを求めるだけ。

```haskell
part2 steps = sum
  [ sum $ zipWith (*) [i, i + i ..] $ map snd ls
  | (i, ls) <- zip [1 ..] $ elems arr ]
```
