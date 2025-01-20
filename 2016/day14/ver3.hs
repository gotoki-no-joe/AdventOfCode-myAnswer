import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.List.Split
import Data.Word
import Data.Char
import Data.Bits
import Data.List

import Debug.Trace

-- 4ビット数値列から、長さ3の同じ値の並びがあるとき、その先頭のものだけ、ビットを立てた16ビット値を返す
firstcontinuous3 :: String -> Word16
firstcontinuous3 str =
  foldl' (.|.) 0 $
  map (bit . digitToInt . head) $
  take 1 $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 3 1 str

-- 4ビット数値列から、長さ5の同じ値の並びがあるとき、そのビットが1になった16ビット値を返す
continuous5 :: String -> Word16
continuous5 str =
  foldl (.|.) 0 $
  map (bit . digitToInt . head) $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 5 1 str

idxSeq :: String -> [(Int,Word16,Word16)]
idxSeq salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let hash = show $ md5 $ BSL.pack $ salt ++ show i
  , let c3 = firstcontinuous3 hash, c3 /= 0
  , let c5 = continuous5 hash
 ]

genKeys salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds ]

sample = "abc"
input = "jlmsuwbz"

test1 = take 64 $ genKeys sample
main1 = take 64 $ genKeys input

-- パート2
md52017 :: String -> String
md52017 str = iterate (show . md5 . BSL.pack) str !! 2017

idxSeq2 :: String -> [(Int,Word16,Word16)]
{-
idxSeq2 salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let hash = md52017 $ salt ++ show i
  , let c3 = firstcontinuous3 hash, c3 /= 0
  , let c5 = continuous5 hash
 ]
-}
-- うまくいかない
-- concat $ runEval $ parList rseq $ chunksOf 8 $
idxSeq2 salt =
  [ (i, c3, c5)
  | (i, hash) <- zip [0 ..] $ map (md52017 . (salt ++) . show) [0 ..]
  , let c3 = firstcontinuous3 hash, c3 /= 0
  , let c5 = continuous5 hash
 ]

genKeys2 salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq2 salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds
  , traceShow i True
  ]

test2 = (!! 63) $ genKeys2 sample
main2 = (!! 63) $ genKeys2 input

main = do
  print test2
  print main2
