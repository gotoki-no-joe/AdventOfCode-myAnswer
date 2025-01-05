# 入力

文字の二次元配列にするぐらいか。
欄外へのアクセスを防止するために、周囲をぐるりと番兵で囲んでおくと扱いがよいが、
欄外へのアクセスにダミーデータを返すアクセス関数を挟むでもできる。
ぐるりと番兵で囲む方法にしておく。

```haskell
import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      ls1 = pad (replicate (w + 2) '.') $ map (pad '.') ls
      fld = listArray ((0,0),(succ h,succ w)) $ concat ls1
  print $ f h w fld

pad x xs = x : xs ++ [x]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Arrau (Int,Int) Char -> ...
part1 fld = ...
```

# パート1

数字の列は左から右に読むので、そのような列の先頭の数字を見つける。
つまり左に数字のない数字。

そこから数字が続く限りの範囲を見つける。
それらの数字の周囲8マスに、数字でも`.`でもない記号があるかを調べる。
一つでもあれば、これが探していた数字列の一つとなる。

```haskell
import Data.Char

part1 h w fld = sum
  [ read $ map (fld !) ijs
  | ij <- range ((1,1),(h, w))
  , isDigit $ fld ! ij
  , not $ isDigit $ fld ! left ij
  , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
  , any (symbol . (fld !)) $ concatMap neighbors ijs ]

left  (i,j) = (i, pred j)
right (i,j) = (i, succ j)

neighbors (i,j) = [(p,q) | p <- [pred i .. succ i], q <- [pred j .. succ j], (i,j) /= (p,q)]

symbol c = not (isDigit c) && c /= '.'
```

# パート2

どうすればいいのやら。
パート1のように数を見つけたとき、その周囲のギア `*` 全てについて、自分の値をぶら下げる。

あとでギアのそれぞれについて、ぶら下がった値がちょうど2個あるものを選び出す。

```haskell
import Data.List

part2 h w fld = sum [a * b | [a,b] <- elems gnums]
  where
    gnums = accumArray (flip (:)) [] ((1,1),(h,w))
      [ (gearPos, val)
      | ij <- range ((1,1),(h, w))
      , isDigit $ fld ! ij
      , not $ isDigit $ fld ! left ij
      , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
      , let val = read $ map (fld !) ijs
      , gearPos <- nub $ filter (('*' ==) . (fld !)) $ concatMap neighbors ijs ]
```

`nub`はひどいかな…

# 丁寧に周囲のマスを取り出す

数字の続いている座標の左から順のリストがあれば、ぐるりのマスの座標を重複なく列挙することはもちろんできる。

```haskell
neighborss ijs =
    (i, pred jL) : (i, succ jR) :
    [(i1,j) | i1 <- [pred i, succ i], j <- [pred jL .. succ jR]]
  where
    (i,jL) = head ijs
    (_,jR) = last ijs

part1a h w fld = sum
  [ read $ map (fld !) ijs
  | ij <- range ((1,1),(h, w))
  , isDigit $ fld ! ij
  , not $ isDigit $ fld ! left ij
  , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
  , any (symbol . (fld !)) $ neighborss ijs ]

part2a h w fld = sum [a * b | [a,b] <- elems gnums]
  where
    gnums = accumArray (flip (:)) [] ((1,1),(h,w))
      [ (gearPos, val)
      | ij <- range ((1,1),(h, w))
      , isDigit $ fld ! ij
      , not $ isDigit $ fld ! left ij
      , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
      , let val = read $ map (fld !) ijs
      , gearPos <- filter (('*' ==) . (fld !)) $ neighborss ijs ]
```
