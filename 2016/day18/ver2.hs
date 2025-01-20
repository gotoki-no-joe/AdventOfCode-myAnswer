{-
2025-1-20 for spoiler

ver1を実行テスト
ghci> phase1 input 40
1978
(0.02 secs, 1,056,512 bytes)
ghci> phase1 input 400000
20003246
(8.22 secs, 9,929,848,056 bytes)

文字のリストでそのままやる、でも10秒かからずにできるようになっている。

そして気になる、トラップが発生する条件は、
**現在位置の状態は関係なく**
**両隣が違うこと**

なんと。
そして、違うときに1になる演算といえばxorなので、
入力100文字に対応する100ビットのIntegerに対して、
shiftL 1 と shiftR 1 を xor した結果が、次の状況を与える。
popCountで罠の個数を数え、幅から引いたら床の数がわかる。
-}
--import Data.List
import Data.Bits

import qualified Data.Map as M

encode2 :: String -> Integer
encode2 str = foldl setBit 0 [i | (i, '^') <- zip [0 ..] str]

decode2 :: Int -> Integer -> String
decode2 w bs = [if testBit bs i then '^' else '.' | i <- [0 .. pred w]]

next2 :: Int -> Integer -> Integer
next2 w bs = clearBit (xor (shiftL bs 1) (shiftR bs 1)) w

observe2 :: String -> Int -> IO ()
observe2 str h = putStr $ unlines $ map (decode2 w) $ take h $ iterate (next2 w) $ encode2 str
  where
    w = length str

compute2 str steps = w * steps - traps
  where
    traps = sum $ map popCount $ take steps $ iterate (next2 w) $ encode2 str
    w = length str

test1a = compute2 sample2 10
main1a = compute2 input 40
main2a = compute2 input 400000

{-
decode2 :: Int -> Integer -> String
decode2 w bs = [if testBit bs i then '^' else '.' | i <- [0 .. pred w]]

step :: Int -> Integer -> Integer
step w bs = clearBit (xor (shiftL bs 1) (shiftR bs 1)) w

-}

sample1 = "..^^."
sample2 = ".^^.^.^^^^"
input = "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

{-
ghci> putStr $ observe sample1 3
..^^.
.^^^^
^^..^
ghci> putStr $ observe sample2 10
.^^.^.^^^^
^^^...^..^
^.^^.^.^^.
..^^...^^^
.^^^^.^^.^
^^..^.^^..
^^^^..^^^.
^..^^^^.^^
.^^^..^.^^
^^.^^^..^^
(0.01 secs, 311,440 bytes)
ghci> test1
38
(0.00 secs, 70,640 bytes)
ghci> main1
1978
(0.00 secs, 128,320 bytes)
ghci> main2
20003246
(0.41 secs, 310,498,792 bytes)
-}

kaburi = cnts
  where
    im = M.fromListWith (+) [(bs, 1) | bs <- take 400000 $ iterate (next2 (length input)) $ encode2 input]
    cnts = M.fromListWith (+) [(k, 1) | k <- M.elems im]

-- 素朴な解法

next1 bs = zipWith (==) (True:bs) (drop 1 bs ++ [True])

encode1 = map ('.' ==)
decode1 = map (\b -> if b then '.' else '^')

observe1 str n = putStr $ unlines $ map decode1 $ take n $ iterate next1 $ encode1 str

compute1 str n = sum $ map (length . filter id) (take n $ iterate next1 $ encode1 str)

test1 = compute1 sample2 10
part1 = compute1 input 40
part2 = compute1 input 400000
