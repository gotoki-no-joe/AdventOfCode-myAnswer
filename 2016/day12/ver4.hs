-- 2025/1/17 for spoiler ver3の性能の悪さに驚愕した。

import Data.Array.IO
import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let prog = listArray (1, length ls) $ zipWith parse [1..] ls
  f prog

isReg (c:cs) = elem c "abcd" && null cs

parse :: Int -> String -> IOUArray Char Int -> IO Int
parse ip l =
  case words l of
    ['c':_, x, y:_] | isReg x -> \v -> readArray v (head x) >>= writeArray v y >> return (succ ip)
                  | otherwise -> \v ->                 writeArray v y (read x) >> return (succ ip)
    ['i':_, x:_]              -> \v -> readArray v x >>= writeArray v x . succ >> return (succ ip)
    ['d':_, x:_]              -> \v -> readArray v x >>= writeArray v x . pred >> return (succ ip)
    ['j':_, x, y]   | isReg x -> \v -> readArray v (head x) >>= \a -> return (if a == 0 then succ ip else read y + ip)
                | read x == 0 -> \v -> return (succ ip)
                | otherwise   -> \v -> return (read y + ip)

--run :: Array Int (STUArray s Char Int -> ST s Int) -> [Int] -> Int
run prog (xs :: [Int]) =
  do
    v <- newListArray ('a','d') xs
    res <- loop v (fst $ bounds prog)
    print res
  where
    loop v ip
      | inRange (bounds prog) ip = (prog ! ip) v >>= loop v
      | otherwise =  readArray v 'a'

part1 prog = run prog [0,0,0,0]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part2 prog = run prog [0,0,1,0]

main2 = runner "input.txt" part2

{-
STArrayがいうこときかないので、IOArray持ち出した。

ghci> test1
42
(0.02 secs, 117,096 bytes)
ghci> main1
317993
(3.81 secs, 3,569,624,616 bytes)
ghci> main2
9227647
(110.93 secs, 103,571,713,704 bytes)
全然変わらない。むしろver3aより遅い。だから、そこじゃない。
-}
