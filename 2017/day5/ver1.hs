import Data.Array

main = do
  fi <- readFile "input.txt"
  let js = (map read $ lines fi) :: [Int]
  let m0 = listArray (0, length js - 1) js
  let trace = compute1 0 m0
  print (length trace - 1)
  putStrLn "part2"
  let t2 = compute2 0 m0
  print (length t2 - 1)

compute1 :: Int -> Array Int Int -> [Int]
compute1 pc mem
  | pc < 0 || snd (bounds mem) < pc = [pc]
  | otherwise = pc : compute1 (pc + ofs) (mem // [(pc,succ ofs)])
  where
    ofs = mem ! pc

sample = compute1 0 $ listArray (0,4) [0,3,0,1,-3]

compute2 :: Int -> Array Int Int -> [Int]
compute2 pc mem
  | pc < 0 || snd (bounds mem) < pc = [pc]
  | 3 <= ofs  = pc : compute2 (pc + ofs) (mem // [(pc,pred ofs)])
  | otherwise = pc : compute2 (pc + ofs) (mem // [(pc,succ ofs)])
  where
    ofs = mem ! pc

sample2 = compute2 0 $ listArray (0,4) [0,3,0,1,-3]

{-
*Main> main
387096
part2
28040648
後半結構時間かった。
-}
