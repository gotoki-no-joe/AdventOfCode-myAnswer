import Data.List.Split
import Data.Bits
import Data.Char

testdata = [3, 4, 1, 5]

input = [31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33]

type State =
  ([Int]  -- ひも : 先頭は現在位置
  ,Int    -- 現在位置
  ,Int    -- スキップサイズ
  )

step :: Int   -- ひもの長さ
     -> State -- 状態
     -> Int   -- 今回ひねる長さ
     -> State
step base (str, cur, skip) len = (ds ++ cs, cur1, skip1) where
  (as,bs) = splitAt len str
  (cs,ds) = splitAt skip (bs ++ reverse as)
  cur1 = cur + len + skip
  skip1 = (skip + 1) `mod` base

run base dat = (bs ++ as, cur1) where
  (str, cur,_) = foldl (step base) ([0..base-1], 0, 0) dat
  cur1 = cur `mod` base
  (as,bs) = splitAt (base - cur1) str

test = run 5 testdata

compute1 = run 256 input

ans1 = product $ take 2 $ fst compute1

{-
後半めちゃめちゃ説明長いんですけど。
-}

-- runの終了処理をなくす
phase dat st = foldl (step 256) st dat
-- 64phaseした後に1回だけ終了処理をする
run2 dat = bs ++ as where
  (str,cur,_) = (!! 64) $ iterate (phase dat) ([0..255], 0, 0)
  cur1 = cur `mod` 256
  (as,bs) = splitAt (256 - cur1) str

dohash :: String -> String
dohash str = dense where
  lens0 = map fromEnum str ++ [17, 31, 73, 47, 23]
  sparse = run2 lens0
  hex16 = map (foldr1 xor) $ chunksOf 16 sparse
  dense = [ intToDigit c | x <- hex16, let (a,b) = divMod x 16, c <- [a,b] ]

test1 = "a2582a3a0e66e6e86e3812dcb672a272" == dohash ""
test2 = "33efeb34ea91902bb2f59c9920caa6cd" == dohash "AoC 2017"
test3 = "3efbe78a8d82f29979031a4aa0b16a9d" == dohash "1,2,3"
test4 = "63960835bcdc130f0b66d7ff4f6a5a8e" == dohash "1,2,4"

ans2 = dohash "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"

{-
やれやれ一発通し。

*Main> ans1
6952
*Main> test1
True
*Main> test2
True
*Main> test3
True
*Main> test4
True
*Main> ans2
"28e7c4360520718a5dc811d3942cf1fd"

-}
