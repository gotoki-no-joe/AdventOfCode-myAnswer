{-# LANGUAGE Strict #-}

-- 2025/1/16 for spoiler

import Data.Array.Unboxed

runner i f = do
  is <- map parse . lines <$> readFile i
  let prog = listArray (1, length is) is
  print $ f prog

ip = pred 'a'
isReg (c:cs) = elem c "abcd" && null cs

--parse :: String -> UArray Char Int -> UArray Char Int
parse l =
  case words l of
    ['c':_, x, y:_] | isReg x -> \v -> accum (flip ($)) v [(ip, succ), (y, const (v ! head x))]
                  | otherwise -> \v -> accum (flip ($)) v [(ip, succ), (y, const (read x))]
    ['i':_, x:_]              -> \v -> accum (flip ($)) v [(ip, succ), (x, succ)]
    ['d':_, x:_]              -> \v -> accum (flip ($)) v [(ip, succ), (x, pred)]
    ['j':_, x, y]   | isReg x -> \v -> accum (flip ($)) v [(ip, if v ! head x == 0 then succ else (read y +))]
                | read x == 0 -> \v -> accum (flip ($)) v [(ip, succ)]
                | otherwise   -> \v -> accum (flip ($)) v [(ip, (read y +))]

type Instr = UArray Char Int -> UArray Char Int
run :: Array Int Instr -> [Int] -> UArray Char Int
run prog xs = loop v0
  where
    v0 = listArray (ip, 'd') $ fst (bounds prog) : xs
    loop v
      | inRange (bounds prog) (v ! ip) = loop $ prog ! (v ! ip) $ v
      | otherwise = v

part1 prog = run prog [0,0,0,0] ! 'a'

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part2 prog = run prog [0,0,1,0] ! 'a'

main2 = runner "input.txt" part2

{-
ver1 : 純インタプリタ方式
ghci> main2
9227647
(74.45 secs, 19,368,611,864 bytes)

ver2 : ハンドアセンブル方式
ghci> part2
9227647
(15.71 secs, 3,263,115,464 bytes)

ver3 : 今回の、セミコンパイル方式
ghci> main2
9227647
(140.67 secs, 97,223,075,776 bytes)

ver1より遅いじゃねぇか。どうなってんだ。
-}

-----------------------------

runner2 i f = do
  ls <- lines <$> readFile i
  let prog = listArray (1, length ls) $ zipWith (parse2 prog) [1..] ls :: Array Int Instr2
  print $ f prog

type Instr2 = UArray Char Int -> Int

parse2 :: Array Int Instr2 -> Int -> String -> Instr2
parse2 arr ip l =
  case words l of
    ['c':_, x, y:_] | isReg x -> \v -> cont (succ ip) $ v // [(y, v ! head x)]
                  | otherwise -> \v -> cont (succ ip) $ v // [(y, read x)]
    ['i':_, x:_]              -> \v -> cont (succ ip) $ accum (flip ($)) v [(x, succ)]
    ['d':_, x:_]              -> \v -> cont (succ ip) $ accum (flip ($)) v [(x, pred)]
    ['j':_, x, y]   | isReg x -> \v -> cont (if v ! head x == 0 then succ ip else read y + ip) v
                | read x == 0 -> cont (succ ip)
                  | otherwise -> cont (read y + ip)
  where
    cont ip1
      | inRange (bounds arr) ip1 = arr ! ip1
      | otherwise = (! 'a')

run2 c prog = prog ! 1 $ v
  where
    v :: UArray Char Int
    v = listArray ('a','d') [0,0,c,0]

test1a = runner2 "sample.txt" (run2 0)
main1a = runner2 "input.txt"  (run2 0)

main2a = runner2 "input.txt" (run2 1)

{-
ghci> test1a
array ('a','d') [('a',42),('b',0),('c',1),('d',0)]
(0.00 secs, 204,632 bytes)
ghci> main1a
array ('a','d') [('a',9227647),('b',5702887),('c',0),('d',0)]
(76.30 secs, 79,579,808,576 bytes)
ghci> main2a
array ('a','d') [('a',9227647),('b',5702887),('c',0),('d',0)]
(90.13 secs, 79,579,808,584 bytes)

       test1  main1  main2
ver1       ?   1.64  74.45
ver2       ?   0.32  15.71
ver3    0.01   3.62 105.71
ver3a   0.01   2.28  72.63
-}
