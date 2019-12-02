import Data.Array

main = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is
    print ans

compute :: [Integer] -> Integer
compute is = (interpret 0 ia) ! 0
  where
    ia = listArray (0, length is - 1) is // [(1,12),(2,2)]

interpret :: Int -> Array Int Integer -> Array Int Integer
interpret pc mem = case mem ! pc of
    1 -> interpret (pc + 4) (mem // [(fromIntegral $ ma (pc + 3), ma (ma (pc + 1)) + ma (ma (pc + 2)))])
    2 -> interpret (pc + 4) (mem // [(fromIntegral $ ma (pc + 3), ma (ma (pc + 1)) * ma (ma (pc + 2)))])
    99 -> mem
    _ -> error "Illegal Opcode"
  where
    ma a = mem ! (fromIntegral a)

test0, test1, test2, test3, test4 :: [Integer]
test0 = [1,9,10,3,2,3,11,0,99,30,40,50]
test1 = [1,0,0,0,99]
test2 = [2,3,0,3,99]
test3 = [2,4,4,5,99,0]
test4 = [1,1,1,4,99,5,6,0,99]

dotest p = elems $ interpret 0 (listArray (0, length p - 1) p)

{- やらかし1 interpret (pc + 4) mem // [(...,...)] と書いて、再帰呼び出しが加工されていないmemで起きていた。型はあってた。 -}
{- やらかし2 パート2の「入力」に当たるコードの書き換えをすっかり忘れていた。
*Main> main
5098658
-}

main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute2 is
    print ans

compute2 :: [Integer] -> Integer
compute2 is = n * 100 + v
  where
    ia0 = listArray (0, length is - 1) is
    (n,v):_ = [ (noun, verb) | noun <- [0..99], verb <- [0..99], 19690720 == interpret 0 (ia0 // [(1,noun),(2,verb)]) ! 0 ]

{-
*Main> main2
5064
-}
