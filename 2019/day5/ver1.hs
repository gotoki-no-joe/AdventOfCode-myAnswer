import Data.Array
import Debug.Trace

main = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is [1]
    print ans

-- RWSモナドでも使うべきか。

compute :: [Integer] -> [Integer] -> [Integer]
compute is js = interpret 0 ia js
  where
    ia = listArray (0, length is - 1) is

interpret :: Int -> Array Int Integer -> [Integer] -> [Integer]
interpret pc mem js = case opcode of
    1 -> arith (+)
    2 -> arith (*)
    3 -> interpret (pc + 2) (memwrite (ma (pc + 1)) (head js)) (tail js)
    4 -> trace (show av1) $ av1 : interpret (pc + 2) mem js
-- new in part2
    5 -> jump (av1 /= 0)
    6 -> jump (av1 == 0)
    7 -> set1 (av1 <  av2)
    8 -> set1 (av1 == av2)
-- </new>
    99 -> []
    x -> error (unwords ["Illegal Opcode", show pc, show x])
  where
    opint = mem ! pc
    opcode = opint `mod` 100
    am1 = opint `div`  100 `mod` 10
    am2 = opint `div` 1000 `mod` 10
    ma a = mem ! (fromIntegral a)
    av1 = (if am1 == 0 then ma else id) (ma (pc + 1))
    av2 = (if am2 == 0 then ma else id) (ma (pc + 2))
    arith op = interpret (pc + 4) (memwrite (ma (pc + 3)) (op av1 av2)) js
    jump True  = interpret (fromIntegral av2) mem js
    jump False = interpret (pc + 3) mem js
    set1 b = interpret (pc + 4) (memwrite (ma (pc + 3)) (if b then 1 else 0)) js
    memwrite a v = mem // [(fromIntegral a, v)]
{-
*Main> main
0
0
0
0
0
0
0
0
0
9219874
[0,0,0,0,0,0,0,0,0,9219874]
-}

main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is [5]
    print ans

{-
*Main> main2
5893654
[5893654]
-}
