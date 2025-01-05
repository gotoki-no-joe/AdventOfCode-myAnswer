# 入力

これはなかなか読み込みの面倒な書式。
いきなり `words` でバラバラにすると後でむしろ面倒そうなので、丁寧にやる。

まず `: ` （コロン、スペース）で切って前後に分ける。
前者を `drop 5` して `Game ` を消したらゲーム番号が読み込める。

後者はさらに `; ` （セミコロン、スペース）で試行ごとに分割する。
それぞれの試行は `, ` （コンマ、スペース）で色ごとに分割し、
それぞれの色についての情報は `words` で分けられる。
取り出した試行は、色ごとの個数を持つレコードに保存してみようか。

```haskell
import Data.List.Split

data R = R {red :: Int, green :: Int, blue :: Int}
  deriving Show -- テスト用

type Game = (Int, [R])

parse :: String -> Game
parse l = (gameID, recs)
  where
    l1 : l2 : _ = splitOn ": " l
    gameID = read $ drop 5 l1
    recs = map mkRec $ splitOn "; " l2
    mkRec l3 = foldr ($) (R 0 0 0)
      [ setCol col $ read num
      | l3 <- splitOn ", " l3
      , let num : col : _ = words l3 ]
    setCol ('r':_) x rec = rec { red = x }
    setCol ('g':_) x rec = rec { green = x }
    setCol ('b':_) x rec = rec { blue = x }
```

とりあえず動かしてみる。

```haskell
runner i f = do
  gs <- map parse . lines <$> readFile i
  print $ f gs
```

```
ghci> runner "sample.txt" (id :: [Game] -> [Game])
[(1,[R {red = 4, green = 0, blue = 3},R {red = 1, green = 2, blue = 6},R {red = 0, green = 2, blue = 0}])
,(2,[R {red = 0, green = 2, blue = 1},R {red = 1, green = 3, blue = 4},R {red = 0, green = 1, blue = 1}])
,(3,[R {red = 20, green = 8, blue = 6},R {red = 4, green = 13, blue = 5},R {red = 1, green = 5, blue = 0}])
,(4,[R {red = 3, green = 1, blue = 6},R {red = 6, green = 3, blue = 0},R {red = 14, green = 3, blue = 15}])
,(5,[R {red = 6, green = 3, blue = 1},R {red = 1, green = 2, blue = 2}])]
```

よさそうだ。

# パート1

ゲームの各セットについて、条件が満たされるものだけを取り出し、そのIDを合計する。
言われるとおり。

```haskell
part1 :: [Game] -> Int
part1 gs = sum [i | (i, recs) <- gs, all prop recs]
  where
    prop r = red r <= 12 && green r <= 13 && blue r <= 14

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
```

# パート2

ゲームのセットを通して、色ごとに最大値を求める。3つの数を掛け合わせ、総和をとる。
言われるとおり。

```haskell
part2 gs = sum
  [ r * g * b
  | (_,recs) <- gs
  , let r = maximum $ map red   recs
  , let g = maximum $ map green recs
  , let b = maximum $ map blue  recs ]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2
```

# データの表現形式について

この問題を解くためだけの書き捨てプログラムとしては、レコード構文を使うことなく、
`[r,g,b] :: [Int]` のように位置に意味を持たせたリストで済ませることもできたが、
それでは、データ表現から要素を取り出すのに `caddr` とかの呪文を駆使する魔法時代に戻ってしまう。
データ構造自体がその意味を説明するような表現を考えるようにしたい。
