# 入力

二つの異なる種類のデータが入力される面倒なタイプ。

まず、デリミタである空行で前半と後半に分ける。
ページ番号に負はないし、あとはいつものように `wordsBy (not . isDigit)` で読み込めばいい。
前半はペアにしておきたい気もするが、リストでも別にいいだろう。

というか、先に数値読み取りを済ませてからの方が楽だ。

```haskell
import Data.List
import Data.List.Split
import Data.Char

runner i f = do
  nss <- map (map read . wordsBy (not . isDigit)) . lines <$> readFile i
  let (nss1, _:nss2) = break null nss
  print $ f nss1 nss2

test1 = runner "sample.txt" part1
main2 = runner "input.txt" part1

part1 :: [[Int]] -> [[Int]] -> Int
part1 rules updates = ...
```

# パート1

## 分かりやすいけど重いやり方

ひとつの更新について、正しいかどうかを判定する。
個々の規則 `x|y` について、更新の中に `x`, `y` が現れる位置を `elemIndex` で調べ、
両方とも存在する場合、位置関係を確認する。
片方でも見つからない場合は、規則が適用されないだけになる。

```haskell
part1 :: [[Int]] -> [[Int]] -> Int
part1 rules = sum . map getCenter . filter (validate rules)

getCenter xs = xs !! div (length xs) 2

validate rules update = and
  [ valid i j
  | l:r:_ <- rules
  , let i = elemIndex l update
  , let j = elemIndex r update]
  where
    valid (Just a) (Just b) = a < b
    valid _ _ = True
```

## あたまの混乱するやり方

前から順にページを確定させていったとき、ページ `x` を確定させると、
規則ではそのページより前になければならないページは、これ以降に入れることはできなくなる。
ページを追加するごとにそのような制約が増えていき、
各時点での制約に反していないページが追加されている限りは問題ない、
とすると、必要なことだけを確認する、より高速な手順ができるだろう。

これを実行するために必要な下準備は、各ページ `x` に対して、
それが右側に現れている規則の左側のページ番号の集合を取り出せるようにしておくことである。
なお、パズル入力ではページ番号は全て2桁の数字のようなので、配列を使う。

```haskell
test1a = runner "sample.txt" part1a
main1a = runner "input.txt" part1a

part1a :: [[Int]] -> [[Int]] -> Int
part1a rules = sum . map getCenter . filter (validate2 ra)
  where
    ra = fmap IS.fromList $ accumArray (flip (:)) [] (1,99) [(r, l) | l:r:_ <- rules]

validate2 ra update = loop IS.empty update
  where
    loop _ [] = True
    loop prohibited (x:xs) = IS.notMember x prohibited && loop (IS.union prohibited $ ra ! x) xs
```

# パート2

規則に反していると判定された更新一つについて、正しい並べ方を作る方法を考える。

## 探索解法

`Data.List.permutations` で順列を全て作り、`validate2` が通るものを選ぶ、というのはいかにも乱暴である。
実際それは間に合わない。

しかし高速な `validate2` の計算を参考にして、
次に印刷するページの候補は、残りのページ集合（自分も含めて良い）と、自分が禁止するページ集合がdisjointであること。
つまり、他のまだ残っているページを誰も禁止にしないものなら選ぶことができる。
（そうでないと、一度禁止されたページはそれ以降決して出せないから。）  
これで全てのページが並べられたら成功である。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [[Int]] -> [[Int]] -> Int
part2 rules = sum . map (getCenter . makeCorrect ra) . filter (not . validate2 ra)
  where
    ra = fmap IS.fromList $ accumArray (flip (:)) [] (1,99) [(r, l) | l:r:_ <- rules]

makeCorrect ra update = head $ loop $ IS.fromList update
  where
    loop upd
      | IS.null upd = [[]]
      | otherwise =
        [ p : qs
        | p <- IS.elems upd, IS.disjoint upd $ ra ! p
        , qs <- loop $ IS.delete p upd]
```

## グラフアルゴリズムによる解法

規則 `x|y` は、`y` よりも `x` が先に選ばれる必要があることを意味している。
このような関係が要素を制約しているとき、
要素をノード、関係を有向辺とするグラフに対するトポロジカルソートが、制約を満たす順序を与える。
これは標準ライブラリ `Data.Graph` にあるので、これを使ってもこの問題は解ける。
