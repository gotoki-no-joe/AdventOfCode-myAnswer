import Data.List
import Data.List.Split
import qualified Data.Array as A

main = do
  fi <- readFile "input.txt"
  let ps = getpoints fi
  let b@((x0,y0),(x1,y1)) = getbounds ps
  let arr = A.array b (makearr b ps)
  let uds = [ arr A.! (x,y) | x <- [x0..x1], y <- [y0,y1] ]
  let lrs = [ arr A.! (x,y) | x <- [x0,x1], y <- [y0..y1] ]
  let excludes = nub (uds++lrs)
  let alls = A.elems arr
  let as = getareas alls (length ps) excludes
  print as
  let a = maxid as
  print a

getpoints :: String -> [(Int,Int)]
getpoints = map (\[at,bt] -> (read at, read bt)) . map (splitOn ", ") . lines

getbounds :: [(Int,Int)] -> ((Int,Int),(Int,Int))
getbounds ps = ((x0, y0),(x3, y3)) where
  (xs,x1,x2) = (map fst ps, minimum xs, maximum xs)
  w = x2 - x1
  (x0,x3) = (x1 - w, x2 + w)
  (ys,y1,y2) = (map snd ps, minimum ys, maximum ys)
  h = y2 - y1
  (y0,y3) = (y1 - h, y2 + h)

{-
上下左右の限界に張り付いている点は、無限の広がりを持つ。
それよりも内側の座標点全てについて、数えることを行う。
-}

makearr ((x0,y0),(x1,y1)) ps =
  [ ((x,y), getnearest (x,y) ps) | x <- [x0..x1] , y <- [y0..y1] ]

getnearest :: (Int,Int) -> [(Int,Int)] -> Int
getnearest (x,y) ps = if singleton ns then head ns else 0 where
  ds = [ abs (x-a) + abs (y-b) | (a,b) <- ps ]
  ne = minimum ds
  ns = [ i | (d,i) <- zip ds [1..], d == ne ]

singleton [_] = True
singleton _ = False

{- 端のやつも調べるときには出てくるのに、最大を決めるときは無視するとか面倒だなこれ。

ギリのboundでは足らない気がする。Bとかみたいな点があるから。
縦横3倍の領域で勘定して、端に触っていない記号だけを気にする、
というアルゴリズムなら充分だろう。
-}

{-
getareas alls n excludes =
    [ (num, i) -- (if elem i excludes then 0 else num, i)
    | i <- [1..n]
    , let num = length $ filter (i ==) alls
    ]
-}

-- こいつが、80万要素 x 50 頂点 の数え上げをするから遅いのか。

getareas alls n excludes =
  [ (i,if elem i excludes then 0 else n) | (i,n) <- ins ] where
    ins = map (\ds -> (head ds, length ds)) $ group $ sort alls

maxid as = [ a | a <- as, snd a == x ] where
  x = maximum $ map snd as

{-
*Main> main
[(0,0),(1,0),(2,0),(3,0),(4,0),(5,2469),(6,0),(7,0),(8,2605),(9,0),(10,1541),(11,2355),(12,0),(13,0),(14,1549),(15,0),(16,0),(17,1579),(18,1208),(19,0),(20,1582),(21,0),(22,4233),(23,0),(24,603),(25,1606),(26,488),(27,3316),(28,3447),(29,0),(30,1307),(31,1671),(32,1946),(33,0),(34,3435),(35,1885),(36,0),(37,0),(38,1289),(39,0),(40,0),(41,3024),(42,0),(43,2237),(44,0),(45,1205),(46,2317),(47,1877),(48,1632),(49,0),(50,0)]
[(22,4233)]
かなり時間がかかる。やはりこういうのは書き換えられる配列がないときついのか。
-}
