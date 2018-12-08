{- 2015 day 4 を再利用 -}

import qualified Data.ByteString as B

import qualified Crypto.Hash.MD5 as MD5

import Data.ByteString.Char8 (pack)

import Text.Printf
import Data.Word

import Data.Char (intToDigit)
import Data.List

prefix = "abbhdwsy"

sample1 = "abc"

enc = concatMap hexprint .B.unpack . MD5.finalize . MD5.update MD5.init . pack

hexprint :: Word8 -> String
hexprint = printf "%02x"

{-
MD5 contextもimmutableなので、
prefixを処理した後は、showした数字の列を追加でやってみればいい。
「初めは1～9を与えて、次からは0～9を与えて、
とContextを残して幅優先探索をしてもいいけれど、
とても面倒なので。
-}

compute1 pfx = take 8
  [ intToDigit $ fromIntegral (w8s !! 2)
  | n <- [1..]
  , let w8s = B.unpack $ MD5.finalize $ MD5.update ctx1 $ pack $ show n
  , w8s !! 0 == 0
  , w8s !! 1 == 0
  , w8s !! 2 < 16
  ]
  where
    ctx1 = MD5.update MD5.init $ pack pfx

ans1 = compute1 prefix

compute2 pfx = map snd $ sort $ take8 [0..7]
  [ (w8s !! 2, intToDigit $ fromIntegral ((w8s !! 3) `div` 16))
  | n <- [1..]
  , let w8s = B.unpack $ MD5.finalize $ MD5.update ctx1 $ pack $ show n
  , w8s !! 0 == 0
  , w8s !! 1 == 0
  , w8s !! 2 <  8
  ]
  where
    ctx1 = MD5.update MD5.init $ pack pfx

take8 [] _ = []
take8 ns (ic@(i,_):ics)
  | elem i ns = ic : take8 (delete i ns) ics
  | otherwise = take8 ns ics

ans2 = compute2 prefix

{-
*Main> compute1 "abc"
"18f47a30"
*Main> ans1
"801b56a7"
*Main> compute2 "abc"
"05ace8e3"
*Main> ans2
"424a0197"

重い重い。
-}
