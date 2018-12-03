{-
(R|L)d+,_の繰り返し、最後はなし
-}

import Data.List.Split
import Control.Monad.Writer
import qualified Data.Set as S

type POS = (Int,Int)
type POSDIR = (POS,(Int,Int))

main = do
  l <- readFile "input.txt"
  let cs = splitOn ", " l
  let ans = compute cs
  print ans
  let manh = manhdis (fst ans)
  print manh
  let ps = execWriter (compute2 cs)
  let collision = findcol ps
  print collision
  print $ manhdis collision

manhdis (a,b) = abs a + abs b

compute cs = foldl walk ((0 :: Int,0 :: Int),(0,1)) cs

rot 'R' (dx,dy) = (dy,-dx)
rot 'L' (dx,dy) = (-dy,dx)

walk ((x,y),(dx,dy)) (d:ds) = ((x+n*dx1,y+n*dy1),(dx1,dy1)) where
  (dx1,dy1) = rot d (dx,dy)
  n = read ds

{-
後半はぜんぜん違うじゃないか！
出現した座標を全て生成して、重複した場所を見つける。
-}

compute2 :: [String] -> Writer [POS] ()
compute2 cs = do
  let initial = ((0,0),(0,1))
  tell [(0,0)]
  compute2sub initial cs

compute2sub :: POSDIR -> [String] -> Writer [POS] ()
compute2sub initial [] = return ()
compute2sub ((x,y),(dx,dy)) ((d:ds):cs) = do
  let (dx1,dy1) = rot d (dx,dy)
  let n = read ds
  tell [ (x+k*dx1, y+k*dy1) | k <- [1..n] ]
  compute2sub ((x+n*dx1,y+n*dy1),(dx1,dy1)) cs

-- 手再帰は避けたかったが、これ以上詰めるのも面倒だった。

findcol :: [POS] -> POS
findcol ps = fcsub ps S.empty

fcsub (p:ps) s
  | S.member p s = p
  | otherwise = fcsub ps (S.insert p s)

{-
*Main> main
((104,-127),(1,0))
231
(-10,-137)
147
-}
