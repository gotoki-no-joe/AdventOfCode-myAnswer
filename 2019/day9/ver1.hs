import Data.IntMap

main = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is
    print ans

{-
メモリを配列でなくIntMapで実装する
-}

compute :: [Integer] -> [Integer]
compute is = interpret 0 0 ia [1]
  where
    ia = fromList $ zip [0..] is

-- pc, base, memory, input, output
interpret :: Int -> Integer -> IntMap Integer -> [Integer] -> [Integer]
interpret pc base mem js = case opcode of
    1 -> arith (+)
    2 -> arith (*)
    3 -> interpret (pc + 2) base (memwrite ad1 (head js)) (tail js)
    4 -> av1 : interpret (pc + 2) base mem js
    5 -> jump (av1 /= 0)
    6 -> jump (av1 == 0)
    7 -> set1 (av1 <  av2)
    8 -> set1 (av1 == av2)
    9 -> interpret (pc + 2) (base + av1) mem js
    99 -> []
    x -> error (unwords ["Illegal Opcode", show pc, show x])
  where
    opint = mem ! pc
    opcode = opint `mod` 100
    am1 = opint `div`   100 `mod` 10
    am2 = opint `div`  1000 `mod` 10
    am3 = opint `div` 10000 `mod` 10
    ma a = findWithDefault 0 (fromIntegral a) mem
    av1 = case am1 of
        0 -> ma (ma $ pc + 1)
        1 -> ma (pc + 1)
        2 -> ma (base + ma (pc + 1))
    av2 = case am2 of
        0 -> ma (ma $ pc + 2)
        1 -> ma (pc + 2)
        2 -> ma (base + ma (pc + 2))
    ad3 = case am3 of
        0 -> ma $ pc + 3
        2 -> base + ma (pc + 3)
    ad1 = case am1 of
        0 -> ma $ pc + 1
        2 -> base + ma (pc + 1)
    arith op = interpret (pc + 4) base (memwrite ad3 (op av1 av2)) js
    jump True  = interpret (fromIntegral av2) base mem js
    jump False = interpret (pc + 3) base mem js
    set1 b = interpret (pc + 4) base (memwrite ad3 (if b then 1 else 0)) js
    memwrite ad v = insert (fromIntegral ad) v mem

main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute2 is
    print ans

compute2 :: [Integer] -> [Integer]
compute2 is = interpret 0 0 (fromList $ zip [0..] is) [2]

test1 = interpret 0 0 (fromList $ zip [0..] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]) []
test2 = interpret 0 0 (fromList $ zip [0..] [1102,34915192,34915192,7,4,7,99,0]) []
test3 = interpret 0 0 (fromList $ zip [0..] [104,1125899906842624,99]) []

{-
*Main> main
[2941952859]
*Main> main2
[66113]
後者は確かに時間がかかる。
-}
