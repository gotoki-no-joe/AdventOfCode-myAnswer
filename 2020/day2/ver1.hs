import Data.List
import Data.Char

main = do
  co <- readFile "input.txt"
  let pws = map parse $ lines co
  let ans = length $ filter check pws
  print ans
  let ans2 = length $ filter check2 pws
  print ans2

-- 数 - 数 spc 文字 : password
parse :: String -> (Int,Int,Char,String)
parse xs = (read ds1, read ds2, c, ws)
  where
    (ds1,'-':xs1) = span isDigit xs
    (ds2,' ':c:':':' ':ws) = span isDigit xs1

check :: (Int,Int,Char,String) -> Bool
check (l,u,c,ws) = l <= n && n <= u
  where
    n = length $ filter (c ==) ws

check2 :: (Int,Int,Char,String) -> Bool
check2 (l,u,c,ws) = (ws1 !! l == c) /= (ws1 !! u == c)
  where
    ws1 = '*':ws

{-
*Main> main
636
588
-}