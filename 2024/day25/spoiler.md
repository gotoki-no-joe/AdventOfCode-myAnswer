# 入力

空行ごとに区切る。
あとは本体に任せよう。

```haskell
runner i f = do
  ents <- wordsBy null . lines <$> readFile i
  print $ f ents
```

# 分析

図面の個数、図面の幅は一定のようだが行数は一定なのか、
図面に重複はないのか、問題文だけでは怪しいので確認する。

```haskell
import Data.List

check ents = (length ents, all (head ls ==) ls, nub ents == ents)
  where
    ls = map length ents
```

```
ghci> runner "sample.txt" check
(5,True,True)
ghci> runner "input.txt" check
(500,True,True)
```

意地悪はないようだ。

# 構築

要求されている **fit** とは、錠と鍵の形がぴったり一致することでは**なく**、
重なりが出てしまわない、という事のようだ。

なので、錠のそれぞれの列にある `#` の数を取り出しておき、
鍵のそれぞれの列の値と足し合わせて全て7を越えないような組み合わせの個数を求めればよい。

```haskell
part1 ents = length
  [ ()
  | e <- ents, head (head e) == '.'
  , let is = process e
  , js <- locks
  , all (7 >=) $ zipWith (+) is js
  ]
  where
    locks = [process e | e <- ents, head (head e) == '#']
    process = map (length . filter ('#' ==)) . transpose

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
```
