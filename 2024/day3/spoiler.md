# 入力

単なる長い一行なので、読み込むしかない。
`readFile`は末尾の改行文字を残すので注意。

```haskell
runner i f = readFile i >>= print . f . init

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: String -> Int
part1 xs = ...
```

正規表現あったら楽な話だねこれ…

# パート1

## 凝らない、普通の、プチ富豪的なやり方

パターンは重なり合うこともないので、`tails` を使って文字列の全ての途中から調べて、
目標のパターンが見つかったら結果を返して集計する、とできそう。

```haskell
import Data.List

part1 xs = sum $ map findit $ tails xs
```

`findit` は、まず先頭4文字を取り出して、それが `mul(` でなければ失敗する。
次に、数字文字が続くだけ切り出して、それが1文字以上3文字以下でなければ失敗する。
その続きが `,` でなければ失敗する。
その次もまた数字文字の連続を切り出す。
最後に `)` を確認する。

```haskell
import Data.Char

findit :: String -> Int
findit xs
  | c1, c2, c3, c4, c5 = read xs3 * read xs5
  | otherwise = 0
  where
    (xs1,xs2) = splitAt 4 xs               -- 先頭4文字が "mul(" で
    c1 = xs1 == "mul("
    (xs3,xs4) = span isDigit xs2           -- つづく数字列が
    l1 = length xs3
    c2 = 1 <= l1 && l1 <= 3                -- 3文字以下で
    c3 = not (null xs4) && head xs4 == ',' -- コンマがあって
    (xs5,xs6) = span isDigit $ tail xs4    -- 3文字までの数字列があって
    l2 = length xs5
    c4 = 1 <= l2 && l2 <= 3
    c5 = not (null xs6) && head xs6 == ')' --カッコ閉じで終わる
```

## カイゼン

確かにこれで答えは出るけど、なんとも書き捨て感がある。
「固定の文字列があるか確認して、あれば続きを返す、なければ失敗する」
「3文字までの数字列があるか確認して、あればその数値と続きを返す、なければ失敗する」
という計算にパターンを括りだしてみよう。

```haskell
str pat xs
  | as == pat = Just bs
  | otherwise = Nothing
  where
    (as,bs) = splitAt (length pat) xs

-- 調べたらこれ Data.List.stripPrefix だった。

num3 xs
  | 1 <= l && l <= 3 = Just (read as, bs)
  | otherwise = Nothing
  where
    (as,bs) = span isDigit xs
    l = length as
```

すると、これを利用する上位の計算は、`Maybe`モナドで流れを制御できる。

```haskell
findit :: String -> Maybe Int
findit xs = do
  xs1 <- stripPrefix "mul("
  (n1, xs2) <- num3 xs1
  xs3 <- stripPrefix "," xs2
  (n2, xs3) <- num3 xs3
  str ")"
  return $ n1 * n2
```

呼び出し元はこうなる。

```haskell
import Data.List

part1 xs = sum $ mapMaybe findit $ tails xs
```

失敗した瞬間に `findit` 全体を放棄できるのはいいが、
「残りの文字列」をいちいち受け取っては次に送るところがダルい。
それは State モナドを使えば配管を裏に通せるはずだ。

```haskell
import Control.Monad
import Control.Monad.State

str :: String -> StateT String Maybe () -- 裏の状態としてStringを持つStateモナドで、
str x = do                              -- 表の実行制御はMaybeモナドで、このアクションの結果は ()
  (s1,s2) <- splitAt (length x) <$> get
  guard $ s1 == x                       -- Maybeモナドで失敗を制御
  put s2

-- モナド全開版
str x = get >>= lift . stripPrefix x >>= put

number :: StateT String Maybe Int
number = do
  (s1,s2) <- span isDigit <$> get
  let dlen = length s1
  guard $ 1 <= dlen && dlen <= 3
  put s2
  return $ read s1

mulCmd :: StateT String Maybe Int
mulCmd = do
  str "mul("
  n1 <- number
  str ","
  n2 <- number
  str ")"
  return $ n1 * n2

findit :: String -> Maybe Int
findit xs = evalStateT mulCmd xs
```

`mulCmd` のコードは、関心のあることだけに集中できてかなりいい感じ。
いつの間にか、簡単なパーサコンビネータみたいになっている。

## 完成

ここまで来たら
- 一度パターンの読み取りに成功したら、その次から続行
- パターンが見つからなければ、1文字は読み飛ばし、さらに `m` でない文字を全て読み飛ばしてから再挑戦
- 文字列が空になったら終わり
- 見つかった積の結果は次に伝えて累積する

という形まで昇華させよう。

```haskell
import Control.Applicative

part1 :: String -> Maybe Int
part1 xs = evalStateT (topLevel 0) xs

endOfFile :: StateT String Maybe ()
endOfFile = do
  s <- get
  guard $ null s

topLevel :: Int -> StateT String Maybe Int
topLevel acc =
  do
    endOfFile
    return acc
  <|>
  do
    n <- mulCmd
    topLevel $! acc + n
  <|>
  do -- 不要な文字読み飛ばし
    modify $ dropWhile ('m' /=) . drop 1
    topLevel acc
```

`topLevel` 内の、不要な文字読み飛ばしの位置関係が繊細で、間違えると分岐が爆発して帰ってこなくなる。どうしてそうなのかちょっとよくわからない。

# パート2

パート1の完成版をベースにして、`topLevel` を disabled なものと相互参照するようにすればすぐできる。

```haskell
test2 = runner "sample2.txt" part2
main2 = runner "input.txt" part2

part2 :: String -> Maybe Int
part2 xs = evalStateT (topLevel2 0) xs

topLevel2 :: Int -> StateT String Maybe Int
topLevel2 acc =
  do
    endOfFile
    return acc
  <|>
  do
    n <- mulCmd
    topLevel2 $! acc + n
  <|>
  do
    str "don't()"
    disabled acc
  <|>
  do
    modify $ dropWhile (\c -> c /= 'm' && c /= 'd') . drop 1
    topLevel2 acc

disabled :: Int -> StateT String Maybe Int
disabled acc =
  do
    endOfFile
    return acc
  <|>
  do
    str "do()"
    topLevel2 acc
  <|>
  do
    modify $ dropWhile ('d' /=) . drop 1
    disabled acc
```
