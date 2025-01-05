# 入力

行ごとの文字列を渡す。

```haskell
runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [String] -> Int
part1 ls = ...
```

# パート1

それぞれの行について、数字だけを全て抜き出し、その数を求めたリストの、先頭と末尾を掛け合わせる。
を全ての行にした結果を足し合わせる。

```haskell
import Data.Char

part1 :: [String] -> Int
part1 = sum . map (d1d2 . getds)
  where
    d1d2 ds = head ds * 10 + last ds
    getds l = [digitToInt c | c <- l, isDigit c]
```

# パート2

例にも `eightwo` のように綴りの重なった例があり、これをどう捉えるのかよくわからないけど、
もしこの二つしか存在しないのだったら確かに両方読み取る必要があるし、
もし他にも候補が含まれているのだったら途中は関係ないので、
どちらにせよ飛ばすよりは拾っておく方がよいだろう。

辞書を用意しておき、行の `tails` それぞれについて、その先頭に辞書の要素が一致するか調べる。
辞書に重なる要素はないので、総当たりで問題ないが、ひとつ発見したら検索を打ち切るように `take 1` をかけよう。
辞書に `abcde` と `abc` の両方の項目があるような場合は、前者を先にチェックするように辞書の前に配置すればよい。

```haskell
dic :: [(String, Int)]
dic = [([intToDigit i], i) | i <- [1 .. 9]] ++
      zip (words "one two three four five six seven eight nine") [1 ..]

part2 :: [String] -> Int
part2 = sum . map (d1d2 . getds)
  where
    d1d2 ds = head ds * 10 + last ds
    getds l =
      [ d
      | tl <- tails l
      , d <- take 1 [d | (s,d) <-dic, isPrefixOf s tl] ]
```

パート1から `getds` だけ変更した。
