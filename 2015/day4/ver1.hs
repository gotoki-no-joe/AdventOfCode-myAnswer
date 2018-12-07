import qualified Data.ByteString as B

import qualified Crypto.Hash.MD5 as MD5

import Data.ByteString.Char8 (pack)

import Text.Printf
import Data.Word

prefix = "iwrupvqb"

sample1 = "abcdef609043"
sample2 = "pqrstuv1048970"

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

compute1 pfx = head
  [ n
  | n <- [1..]
  , let w8s = B.unpack $ MD5.finalize $ MD5.update ctx1 $ pack $ show n
  , w8s !! 0 == 0
  , w8s !! 1 == 0
  , w8s !! 2 < 16
  ]
  where
    ctx1 = MD5.update MD5.init $ pack pfx

ans1 = compute1 prefix

ans2 = head
  [ n
  | n <- [1..]
  , let w8s = B.unpack $ MD5.finalize $ MD5.update ctx1 $ pack $ show n
  , w8s !! 0 == 0
  , w8s !! 1 == 0
  , w8s !! 2 == 0
  ]
  where
    ctx1 = MD5.update MD5.init $ pack prefix

{-
*Main> compute1 "abcdef"
609043
*Main> compute1 "pqrstuv"
1048970
*Main> ans1
346386
*Main> ans2
9958218
-}
