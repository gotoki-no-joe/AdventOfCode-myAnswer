import qualified Data.IntSet as S

main = do
  fi <- readFile "input.txt"
  let ns = map myRead $ lines fi
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

-- 見たことのある周波数の集合、現在の周波数、変化リスト、結果
loop :: S.IntSet -> Int -> [Int] -> Int
loop s c (n:ns)
  | S.member c s = c
  | otherwise = loop (S.insert c s) (c+n) ns

{-
*Main> main
595
part two
80598
-}
