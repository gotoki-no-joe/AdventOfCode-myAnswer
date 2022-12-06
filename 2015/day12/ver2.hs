-- 2022-11-16

import Text.Parsec
import Text.Parsec.Char

data JSON = JSONNum Int | JSONStr String | JSONList [JSON] | JSONObj [(String, JSON)] deriving (Show, Eq)

{-
numP :: Parsec String u JSON
numP = do
  s <- option id (negate <$ char '-')
  ds <- many1 digit
  return $ JSONNum $ s $ read ds
-}

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

test1 = do
  co <- readFile "input.txt"
  print $ runParser jsonP () "" co

traverse1 :: JSON -> [Int]
traverse1 x = inner x []
  where
    inner (JSONNum n) rest = n : rest
    inner (JSONList js) rest = foldr inner rest js
    inner (JSONObj kvs) rest = foldr (inner . snd) rest kvs
    inner _ rest = rest

part1 = do
  co <- readFile "input.txt"
  let Right j = runParser jsonP () "" co
  print $ sum $ traverse1 j

traverse2 :: JSON -> [Int]
traverse2 x = inner x []
  where
    inner (JSONNum n) rest = n : rest
    inner (JSONList js) rest = foldr inner rest js
    inner (JSONObj kvs) rest
      | elem (JSONStr "red") vs = rest
      | otherwise = foldr inner rest vs
      where
        vs = map snd kvs
    inner _ rest = rest

part2 = do
  co <- readFile "input.txt"
  let Right j = runParser jsonP () "" co
  print $ sum $ traverse2 j
