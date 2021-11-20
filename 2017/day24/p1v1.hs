{-
多重辺を含むグラフの、0ノードからの最大スコア経路を、深さ優先探索で求める。
辺にIDを振る。
現在位置、まだ使っていない辺のIDの集合、ここまでのスコアの組が状態。
ノードごとに、そこに接続する辺のIDの集合を持つ。

同じノードに戻るループがある場合、その場で使い切る、という最適化ぐらいはやっておこうか。
面倒だからとりあえずナシで。
-}

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Array

sample :: [(Int,Int)]
sample = [(0,2),(2,2),(2,3),(3,4),(3,5),(0,1),(10,1),(9,10)]

part1 ps = maximum $ dfs step (0,avail0,0)
  where
    n = length ps
    iedges = zip [1..] ps
    edges = array (1,n) iedges
    graphL = IM.fromListWith (++) [x | (i,(a,b)) <- iedges, x <- [(a,[i]),(b,[i])]]
    graph = IM.map IS.fromList graphL
    avail0 = IS.fromList $ [1..n]
    step (node, avails, score) = ([score],[(a,IS.delete e avails,score1+a) |(e,a) <- dests])
      where
        score1 = score + node
        avail = IS.elems $ IS.intersection avails (graph IM.! node)
        dests = [(e, if a == node then b else a) | e <- avail, let (a,b) = edges ! e]

-- @gotoki_no_joe
dfs :: (s -> ([x],[s])) -> s -> [x]
dfs f i = loop [i]
  where
    loop [] = []
    loop (x:xs) = let (a,b) = f x in a ++ loop (b ++ xs)

main1 = do
  co <- readFile "input.txt"
  print $ part1 $ map parseLine $ lines co

parseLine xs = (read as, read bs)
  where
    (as,'/':bs) = span ('/' /=) xs

{-
*Main> part1 sample
31
*Main> main1
1906
-}
