-- 2202-11-12 version

import Data.Array
import Data.List
import Data.Ix

main1 = do
  co <- readFile "input.txt"
  let ans = part1 $ lines co
  print ans

data Cmd = LOff | LOn | LTgl

part1 :: [String] -> Int
part1 ls = length $ filter id $ elems arr
  where
    arr = accumArray control1 False ((0,0),(999,999))
      [ (xy, cmd)
      | (cmd, bnds) <- map parse ls
      , xy <- range bnds
      ]

control1 :: Bool -> Cmd -> Bool
control1 _ LOn  = True
control1 _ LOff = False
control1 b LTgl = not b

parse :: String -> (Cmd, ((Int, Int), (Int, Int)))
parse xs
  | w2 == "on"  = (LOn , (csv w3, xy9))
  | w2 == "off" = (LOff, (csv w3, xy9))
  | otherwise   = (LTgl, (csv w2, xy9))
  where
    (w1:w2:w3:ws) = words xs
    xy9 = csv (last ws)

csv :: String -> (Int, Int)
csv xs = (read as, read bs)
  where
    (as, _:bs) = span (',' /=) xs

main2 = do
  co <- readFile "input.txt"
  let ans = part2 $ lines co
  print ans

part2 ls = sum $ elems arr
  where
    arr = accumArray control2 0 ((0,0),(999,999))
      [ (xy, cmd)
      | (cmd, bnds) <- map parse ls
      , xy <- range bnds
      ]

control2 :: Int -> Cmd -> Int
control2 n LOn = succ n
control2 n LOff = max 0 (pred n)
control2 n LTgl = n + 2
