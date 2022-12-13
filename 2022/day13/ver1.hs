{-
任意に深くリストが入れられるデータ構造
LISPならconsセルだけで作れるのだけど。
-}

import Text.Parsec
import Text.Parsec.String

import Data.List
import Data.List.Split (chunksOf)
import Data.Either

data Packet = PInt Int | PList [Packet] deriving Eq

parsePacket :: Parser Packet
parsePacket =
  PInt . read <$> many1 digit
  <|>
  PList <$> between (char '[') (char ']') (sepBy parsePacket (char ','))

pPacket = parsePacket

instance Show Packet where
  showsPrec p (PInt n) = showsPrec p n
  showsPrec p (PList ps) = showChar '[' . foldr (.) (showChar ']') (intersperse (showChar ',') (map shows ps))

instance Ord Packet where
  compare (PInt x) (PInt y) = compare x y
  compare (PList xs) (PList ys) = compare xs ys
  compare x@(PInt _) (PList ys) = compare [x] ys
  compare (PList xs) y@(PInt _) = compare xs [y]

phase1 fn =
  do
    ilrs <- zip [1..] . chunksOf 2 . map (fromRight undefined . parse parsePacket "") . filter (not . null) . lines <$> readFile fn
    print $ sum $ map fst $ filter (\(i,[l,r]) -> l < r) ilrs

phase1a fn =
  print . sum . map fst . filter (\(i,[l,r]) -> l < r) .
  zip [1..] . chunksOf 2 . map (fromRight undefined . parse pPacket "") . filter (not . null) . lines =<<
  readFile fn

test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

phase2 fn =
  do
    ps <- sort . (div2 :) . (div1 :) . map (fromRight undefined . parse parsePacket "") . filter (not . null) . lines <$> readFile fn
--    mapM_ print ps
    let Just l1 = elemIndex div1 ps
    let Just l2 = elemIndex div2 ps
    print $ succ l1 * succ l2

Right div1 = parse parsePacket "" "[[2]]"
Right div2 = parse parsePacket "" "[[6]]"

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"
