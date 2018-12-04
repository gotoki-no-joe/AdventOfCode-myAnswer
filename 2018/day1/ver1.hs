import Data.List.Split
import qualified Data.IntSet as S

main = do
  fi <- readFile "input.txt"
--  print fi
  let li = endBy "\n" fi
--  print li
  let ns = map myRead li
--  print ns
  let ans = sum ns
  print ans
  putStrLn "part two"
  let ans2 = compute ns
  print ans2

myRead :: String -> Int
myRead ('+':ds) = read ds
myRead ds = read ds

compute :: [Int] -> Int
compute ns = loop S.empty 0 (cycle ns)

loop :: S.IntSet -> Int -> [Int] -> Int
loop s c (n:ns)
  | S.member c s = c
  | otherwise = loop (S.insert c s) (c+n) ns
