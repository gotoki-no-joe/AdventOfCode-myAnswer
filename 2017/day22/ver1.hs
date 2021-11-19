import qualified Data.Vector.Unboxed.Mutable as MVU
import Control.Monad

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

ub = 10000
center = div ub 2

part1 map = do
  putStr ""
  v <- MVU.replicate (ub * ub) False
  let ls = lines map
  let ofs = div (length ls) 2
  forM_ (zip ls [center-ofs..]) (\(l,y) -> do
    forM_ (zip l [center-ofs..]) (\(c,x) ->
      if c == '#' then MVU.write v (xy2p x y) True else return ()
      )
    )
  part1loop v (center,center) (0,-1) 0 10000

part1loop _ _ _ cnt 0 = return cnt
part1loop v (x,y) (dx,dy) cnt rest = do
  c <- MVU.read v (xy2p x y)
  let (dx1,dy1) = if c then (-dy,dx) else (dy,-dx)
  let cnt1 = if c then cnt else succ cnt
  MVU.write v (xy2p x y) (not c)
  let (x1,y1) = (x+dx1,y+dy1)
  if x1 < 0 || ub <= x1 || y1 < 0 || ub <= y1 then error "Run out" else return ()
  part1loop v (x+dx1, y+dy1) (dx1,dy1) cnt1 (pred rest)

xy2p :: Int -> Int -> Int
xy2p x y = y * ub + x

{-
up (0,-1), dn (0,1), ri (1,0), le (-1,0)
turnR (x,y) = (-y,x)
turnL (x,y) = (y,-x)
-}

main1 = do
  co <- readFile "input.txt"
  ans <- part1 co
  print ans

{-
*Main> part1 sample
5587
*Main> main1
5196
小さすぎて信じられなかった。けど10000歩の半分ならそれで普通か。
-}
