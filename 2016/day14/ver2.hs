{-
もっと効率よく求めることができるとありがたい。

MD5 digest を一つ求めるたびに、
それが同じ数字の3つならびを含む数字の一覧、5つ並びを含む数字の一覧、の両方が作れる。
(数, 3数字列, 5数字列) 全くない数は捨てて、こういうタプルの無限リストにできる
数字列でやるより、16ビットのビットマスク2つでやるのがいいのかな。
-}

-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString as BSW
import Data.Digest.Pure.MD5
import Data.Bits
import Data.List
import Data.List.Split

import Data.Word

import Debug.Trace

import Control.Parallel.Strategies

sample = "abc"
input = "jlmsuwbz"

-- ByteString で digest を生成
-- 4ビットずつ128ビットのそれを調べて、3つ同じ並びがあるだの、5つあるだのを知るにはどうしたらいい。

idxSeq :: String -> [(Int,Word16,Word16)]
idxSeq salt =
  [ (i, c3, c5)
  | i <- [0 ..]
  , let bs = md5DigestBytes $ md5 $ BSL.pack $ salt ++ show i
  , let fours = concat [[c .>>. 4, c .&. 15] | c <- BSW.unpack bs]
  , let c3 = continuous3 fours, c3 /= 0
  , let c5 = continuous5 fours
 ]

-- 4ビット数値列から、長さ3の同じ値の並びがあるとき、その先頭のものだけ、ビットを立てた16ビット値を返す
continuous3 :: [Word8] -> Word16
continuous3 fours =
  foldl (.|.) 0 $
  map (bit . fromIntegral . head) $
  take 1 $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 3 1 fours

-- 4ビット数値列から、長さ5の同じ値の並びがあるとき、そのビットが1になった16ビット値を返す
continuous5 :: [Word8] -> Word16
continuous5 fours =
  foldl (.|.) 0 $
  map (bit . fromIntegral . head) $
  filter (\(x:xs) -> all (x ==) xs) $
  divvy 5 1 fours

-- idxSeqに出現する数の中で、そこから1000までの後続のものが、c3 .&. c5 /= 0 となるものがあればキー。
-- そういうindexを64個見つけて、その64個めを返す

--genKeys :: String -> [Int]
genKeys salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds ]

test1 = take 64 $ genKeys sample
main1 = take 64 $ genKeys input

-- パート2
-- 大変なことになった。多分キャッシュは意味ない、と思う。

idxSeq2 :: String -> [(Int,Word16,Word16)]
idxSeq2 salt = concatMap (runEval . parList rpar) $ chunksOf 10
  [ (i, c3, c5)
  | i <- [0 ..]
  , let bs = md5many $ BSL.pack $ salt ++ show i
  , let fours = concat [[c .>>. 4, c .&. 15] | c <- map (fromIntegral . fromEnum) $ BSW.unpack $ md5DigestBytes bs]
  , let c3 = continuous3 fours, c3 /= 0
  , let c5 = continuous5 fours
 ]

md5many xs = iterate (md5 . BSL.pack . show) (md5 xs) !! 2016

genKeys2 salt =
  [ i
  | (i,c3,_):jdds <- tails $ idxSeq2 salt
  , any (\(_,_,c5) -> c3 .&. c5 /= 0) $ takeWhile (\(j,_,_) -> j <= i + 1000) jdds
  , traceShow i True ]

test2 = take 64 $ genKeys2 sample
main2 = take 64 $ genKeys2 input

main = do
  print test2
  print main2
