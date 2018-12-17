{-# Language Strict #-}

import Data.Word
import qualified Data.IntMap as M
import qualified Data.ByteString as B

{-
入力ファイルに二通りの情報が入っている珍しいパターンだ。

後半で案の定メモリが飛んだので、もっと効率的な表現を使う。
ByteStringでやってみよう。

-}

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let gen0 = adjust (0, B.pack $ map c2w8 $ drop 15 $ head ls)
  let rule = makeRule $ map parse $ drop 2 ls
  let gen20 = head $ drop 20 $ iterate (step rule) gen0
  let ans1 = score gen20
  print ans1
  let genFB = head $ drop 50000000000 $ iterate (step rule) gen0
  let ans2 = score genFB
  print ans2

c2w8 '#' = 1
c2w8 '.' = 0

zero4 = B.pack [0,0,0,0]
zero3 = B.pack [0,0,0]
zero2 = B.pack [0,0]

adjust :: (Int,B.ByteString) -> (Int,B.ByteString)
adjust (ofs, pots) = (o1,p2) where
  (o1,p1) =      if B.index pots 0 == 1 then (ofs-4, B.append zero4 pots)
            else if B.index pots 1 == 1 then (ofs-3, B.append zero3 pots)
            else if B.index pots 2 == 1 then (ofs-2, B.append zero2 pots)
            else if B.index pots 3 == 1 then (ofs-1, B.cons 0 pots)
            else                           (ofs,pots)
  n = B.length p1
  p2 =           if B.index p1 (n-1) == 1 then B.append p1 zero4
            else if B.index p1 (n-2) == 1 then B.append p1 zero3
            else if B.index p1 (n-3) == 1 then B.append p1 zero2
            else if B.index p1 (n-4) == 1 then B.snoc p1 0
            else                               p1

parse :: String -> (String,Char)
parse cs = (take 5 cs, last cs)

makeRule :: [(String,Char)] -> M.IntMap Word8
makeRule lrs = M.fromList
  [ (lv, c2w8 r) | (l,r) <- lrs, let lv = sum $ zipWith (*) (map c2w8 l) [16,8,4,2,1] ]

step :: M.IntMap Word8 -> (Int,B.ByteString) -> (Int,B.ByteString)
step rule (ofs, pots) = adjust (ofs+2, B.pack bs) where
  bs =
    [ rule M.! p
    | i <- [0..B.length pots - 5]
    , let p = fromIntegral $ sum $ zipWith (*) [16,8,4,2,1] [ B.index pots k | k <- [i..i+4] ]
    ]

score :: (Int,B.ByteString) -> Int
score (ofs, pots) = sum $ zipWith f [ofs..] (B.unpack pots) where
  f a b = a * fromIntegral b
