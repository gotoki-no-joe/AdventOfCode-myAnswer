import Data.Array
import Data.Char
import qualified Data.IntMap as IM
import qualified Data.Sequence as Q
import qualified Data.Map as M
import Data.List

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1a
main1 = runner "input.txt" part1a

part1a ls = [ans1,ans2,ans3]
  where
-- 迷路のサイズ
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
-- 迷路の文字がひける配列
    grid = listArray bnds $ concat ls
-- 各数字の位置を指すIntMap
    posIM = IM.fromList [(digitToInt c, p) | (p,c) <- assocs grid, isDigit c]
    unvisited = listArray bnds $ repeat False
-- 各数字間の距離を持つMap
    dists :: M.Map (Int,Int) Int
    dists = M.fromList [((i,j), d) | i <- IM.keys posIM, (j, d) <- bfs unvisited (Q.singleton (posIM IM.! i, 0))]
-- 数字間の距離を数える幅優先探索
    bfs _is Q.Empty = []
    bfs vis ((pos, dist) Q.:<| q)
      | vis ! pos = bfs vis q
      | isDigit $ grid ! pos = (digitToInt $ grid ! pos, dist) : bfs vis1 (q Q.>< q1)
      | otherwise = bfs vis1 (q Q.>< q1)
      where
        vis1 = vis // [(pos,True)]
        q1 = Q.fromList [(pos1, succ dist) | pos1 <- neighbors pos, not $ vis ! pos1, grid ! pos1 /= '#']
    neighbors (i,j) = filter (inRange bnds) [(pred i,j),(succ i,j),(i, pred j),(i, succ j)]
-- 0から出て全てを訪問する経路の最短距離を探す深さ優先探索
    ans1 = dfs 0 $ delete 0 $ IM.keys posIM
    dfs _ [] = 0
    dfs u vs = minimum [dists M.! (u,v) + dfs v (delete v vs) | v <- vs]
-- パート2 最後まで行ったら0に戻れ、と。
    ans2 = dfs2 0 $ delete 0 $ IM.keys posIM
    dfs2 u [] = dists M.! (u,0)
    dfs2 u vs = minimum [dists M.! (u,v) + dfs2 v (delete v vs) | v <- vs]
-- これじゃおかしい気がする。
-- なので総当たりする。
    ans3 = minimum [dists M.! (0, last us) + sum (zipWith (curry (dists M.!)) (0:us) us) | us <- permutations $ delete 0 $ IM.keys posIM]
-- 結果は同じだった。そうか。

{-
2024に挑戦。
ステップ1
それぞれの番号から、別の番号で行き止まりになる形でBFSして距離を求める。

頂点はたかだか10個だけど出発点固定なので、9! = 362,880 とおりの巡回経路総当たりして距離を数えて、
（これはDFSですると効率的かな）
最短経路の距離を求める。
すごく素直な問題だった。敬遠してたのが意味不明。

距離を測ってみたら、ポイントは0から7の8個しかなかったわ。

ghci> test1
14
ghci> main1
502

さぁどうだろ。正解。
-}
