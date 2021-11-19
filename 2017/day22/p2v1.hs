import qualified Data.Vector.Unboxed.Mutable as MVU
import Control.Monad
import Data.Word

sample = "..#\n#..\n..."

{-
infectedなマスの位置を座標で管理し、その集合で局面を表現するのは、広くなると無駄が多い。

あるサイズ、例えば32x32の区画に区切って、全て0な場所は持たない、
何らかの情報のある区画の番号から、その区画の詳細情報へのMapがまずあって、
その中身も、複数の表現形式がある、とかだとカッコイイが、面倒だ。

かならず右か左に向きを変えることを考えると、
10000ステップあっても、直線で10000歩は移動できない。
10000x10000のマップの中央から始めれば、まず大丈夫なのではないかと。

そして、時系列で更新するので、Mutable Vectorを使うしかないよなと。
-}

-- ub = 10000000
ub = 100000
center = div ub 2

-- data Node = Clean | Weak | Infect | Flag deriving Eq
stClean, stWeak, stInfect, stFlag :: Word8
stClean  = 0
stWeak   = 1
stInfect = 2
stFlag   = 3

part2 map time = do
  putStr ""
  v <- MVU.replicate (ub * ub) stClean
  let ls = lines map
  let ofs = div (length ls) 2
  forM_ (zip ls [center-ofs..]) (\(l,y) -> do
    forM_ (zip l [center-ofs..]) (\(c,x) ->
      if c == '#' then MVU.write v (xy2p x y) stInfect else return ()
      )
    )
  part2loop v (center,center) (0,-1) 0 time

part2loop _ _ _ cnt 0 = return cnt
part2loop v (x,y) dir cnt rest = do
  c <- MVU.read v (xy2p x y)
  let dir1@(dx1,dy1) = turn c dir
  let c1 = next c
  let cnt1 = if c1 == stInfect then succ cnt else cnt
  MVU.write v (xy2p x y) c1
  let xy1@(x1,y1) = (x+dx1,y+dy1)
  if x1 < 0 || ub <= x1 || y1 < 0 || ub <= y1 then error "Run out" else return ()
  part2loop v xy1 dir1 cnt1 (pred rest)

xy2p :: Int -> Int -> Int
xy2p x y = y * ub + x

turn 0 (dx,dy) = ( dy,-dx)
turn 1 dir     = dir
turn 2 (dx,dy) = (-dy, dx)
turn 3 (dx,dy) = (-dx,-dy)

next 3 = 0
next n = succ n

{-
up (0,-1), dn (0,1), ri (1,0), le (-1,0)
turnR (x,y) = (-y,x)
turnL (x,y) = (y,-x)
-}

main2 = do
  co <- readFile "input.txt"
  ans <- part2 co 10000000
  print ans

{-
曲がり方変えるなよ。
*Main> part2 sample 100
26
*Main> part2 sample 10000000
2511944
*Main> main2
2511633
うん、力押しです。
-}