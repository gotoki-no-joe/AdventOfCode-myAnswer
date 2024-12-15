import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Control.Applicative

import Data.Maybe

import Debug.Trace

runner i f = readFile i >>= print . f . init

-- 素朴なやり方

test1a = runner "sample.txt" part1a
main1a = runner "input.txt" part1a

part1a xs = sum $ map findit $ tails xs

findit :: String -> Int
findit xs
  | c1, c2, c3, c4, c5 = read xs3 * read xs5
  | otherwise = 0
  where
    (xs1,xs2) = splitAt 4 xs
    c1 = xs1 == "mul("
    (xs3,xs4) = span isDigit xs2
    l1 = length xs3
    c2 = 1 <= l1 && l1 <= 3
    c3 = not (null xs4) && head xs4 == ','
    (xs5,xs6) = span isDigit $ tail xs4
    l2 = length xs5
    c4 = 1 <= l2 && l2 <= 3
    c5 = not (null xs6) && head xs6 == ')'

-- ほぼパーザコンビネータ

test1b = runner "sample.txt" part1b
main1b = runner "input.txt" part1b

part1b :: String -> Int
part1b xs = sum $ mapMaybe finditb $ tails xs

number :: StateT String Maybe Int
number = do
  (s1,s2) <- span isDigit <$> get
  let dlen = length s1
  guard $ 1 <= dlen && dlen <= 3
  put s2
  return $ read s1

str :: String -> StateT String Maybe ()
str x = get >>= lift . stripPrefix x >>= put

{-
str x = do
  (s1,s2) <- splitAt (length x) <$> get
  guard $ s1 == x
  put s2
-}

mulCmd :: StateT String Maybe Int
mulCmd = do
  str "mul("
  n1 <- number
  str ","
  n2 <- number
  str ")"
  return $ n1 * n2

finditb :: String -> Maybe Int
finditb xs = evalStateT mulCmd xs

-- 続きからやる形にしたい

test1c = runner "sample.txt" part1c
main1c = runner "input.txt" part1c

part1c :: String -> Maybe Int
part1c xs = evalStateT (topLevel 0) xs

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
  do
    modify $ dropWhile ('m' /=) . drop 1
    topLevel acc

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
