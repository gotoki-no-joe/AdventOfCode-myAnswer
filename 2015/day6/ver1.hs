-- import Data.Char
import Data.Array

data Cmd = LOff | LOn | LToggle deriving Show

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let cmds = expand ls
--  let arr0 = accumArray exec False ((0,0),(999,999)) cmds
--  let ans1 = length $ filter id $ elems arr0
--  print ans1
  let arr2 = accumArray exec2 0 ((0,0),(999,999)) cmds
  let ans2 = sum $ elems arr2
  print ans2

parse :: String -> (Cmd,Int,Int,Int,Int)
parse cs = (c, read x1, read y1, read x2, read y2) where
  (c,x) = case cs !! 6 of
    'n' -> (LOn, 8)
    'f' -> (LOff, 9)
    ' ' -> (LToggle, 7)
  (x1, ',':cs1) = break (',' ==) $ drop x cs
  (y1, cs2) = break (' ' ==) cs1
  (x2, ',':y2) = break (',' ==) $ drop 9 cs2

expand :: [(Cmd,Int,Int,Int,Int)] -> [((Int,Int),Cmd)]
expand ls = [ ((x,y),c) | (c, x1, y1, x2, y2) <- ls, x <- [x1..x2], y <- [y1..y2] ]

exec :: Bool -> Cmd -> Bool
exec _ LOn = True
exec _ LOff = False
exec b LToggle = not b

exec2 :: Int -> Cmd -> Int
exec2 n LOn = succ n
exec2 n LOff = max 0 $ pred n
exec2 n LToggle = n+2

{-
> main
377891
> main
14110788
-}
