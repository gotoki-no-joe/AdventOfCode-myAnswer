# 入力

JSONライブラリを使ってもいいが、この程度の構文は自分で読み込んでしまおう。
まず、JSONデータとは、数字列、ダブルクオートで囲まれた文字列、角括弧とコンマによる配列、
波括弧と文字列とコロンとコンマによるオブジェクトである。

```haskell
import Text.Parsec

data JSON = JSONNum Int | JSONStr String | JSONList [JSON] | JSONObj [(String, JSON)]

numP :: Parsec String u JSON
numP = do
  s <- option id (negate <$ char '-')
  JSONNum . s . read <$> many1 digit

strP :: Parsec String u JSON
strP = JSONStr <$> between (char '"') (char '"') (many1 letter)

lstP :: Parsec String u JSON
lstP = JSONList <$> between (char '[') (char ']') (sepBy jsonP (char ','))

objP :: Parsec String u JSON
objP = JSONObj <$> between (char '{') (char '}') (sepBy colonedP (char ','))
  where
    colonedP = do
      k <- between (char '"') (char '"') (many1 letter)
      char ':'
      v <- jsonP
      return (k,v)

jsonP :: Parsec String u JSON
jsonP = choice [try numP, try strP, try lstP, objP]

allP :: Parsec String u JSON
allP = do
  j <- jsonP
  eof
  return j
```

```haskell
runner f = do
  co <- readFile "input.txt"
  let Right j = runParser allP () "" co
  print $ f $ j
```

# パート1

`JSON`型の値が読み込めたら、走査して数値を全て足し合わせる。

```haskell
part1 = runner traverse
  where
    traverse :: JSON -> Int
    traverse (JSONNum n)   = n
    traverse (JSONStr _)   = 0
    traverse (JSONList js) = sum $ map traverse js
    traverse (JSONObj kvs) = sum $ map (traverse . snd) kvs
```

# パート2

走査の際に、オブジェクトに遭遇した場合、値に `"red"` があれば引き返す。

```haskell
part2 = runner traverse
  where
    traverse :: JSON -> Int
    traverse (JSONNum n)   = n
    traverse (JSONStr _)   = 0
    traverse (JSONList js) = sum $ map traverse js
    traverse (JSONObj kvs)
      | any isRed vs = 0
      | otherwise    = sum $ map traverse vs
      where
        vs= map snd kvs
    isRed :: JSON -> Bool
    isRed (JSONStr "red") = True
    isRed _ = False
```
