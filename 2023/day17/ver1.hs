{-
それぞれのマスについて、タテに到着する、横に到着する、の2つのノードを用意して、
到着したのと違う向きにのみ出発可能にする。
距離1,2,3の直進について、コストを重み付けする。
というグラフを作って、ダイクストラ法で解く感じかな？

スタートの時はどちら向きにも動ける。
-}
import qualified Data.Heap as H
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad.ST
import Control.Monad

import Data.Char
import Data.Array

part12 fn body = do
  ls <- lines <$> readFile fn
  print $ body ls

solve1 ls = minimum
    [dv1 UV.! index bnds (h,w,False), dv1 UV.! index bnds (h,w,True)
    ,dv2 UV.! index bnds (h,w,False), dv2 UV.! index bnds (h,w,True)]
  where
    h = length ls
    w = length $ head ls
    marr = listArray ((1,1),(h,w)) $ map digitToInt $ concat ls
    bnds = ((1,1,False),(h,w,True))  -- 垂直、水平
    cnt = rangeSize bnds
    dv1 = dijkstraV cnt (g !) (index bnds (1,1,False))
    dv2 = dijkstraV cnt (g !) (index bnds (1,1,True))

    g = accumArray (flip (:)) [] (0, pred $ rangeSize bnds)
        [ (index bnds (i,j,d), (index bnds (i1,j1,not d), wt))
        | i <- [1..h], j <- [1..w], d <- [False, True]
        , ijs0 <- ijd i j d
        , let ijs = takeWhile (inRange ((1,1),(h,w))) ijs0
        , let wts = scanl1 (+) $ map (marr !) ijs
        , ((i1,j1), wt) <- zip ijs wts
        ]

    ijd i j True  = [[(i+1,j),(i+2,j),(i+3,j)],[(i-1,j),(i-2,j),(i-3,j)]]
    ijd i j False = [[(i,j+1),(i,j+2),(i,j+3)],[(i,j-1),(i,j-2),(i,j-3)]]

-- @gotoki_no_joe
dijkstraV ::  Int                           -- 頂点数
         -> (Int -> [(Int,Int)])            -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                           -- 開始点
         -> UV.Vector Int  -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstraV n graph start = runST (action graph)
  where
    action graph = do
      dist <- MUV.replicate n maxBound
      MUV.write dist start 0
      let queue = H.singleton (H.Entry 0 start)
      loop graph dist queue
      UV.freeze dist
    loop _ _ queue
      | H.null queue = return ()
    loop graph dist queue = do
      let Just (H.Entry cost u, queue1) = H.uncons queue
      du <- MUV.read dist u
      if du < cost then loop graph dist queue1 else do
        vds <- forM (graph u) (\(v, len) -> do
          let d1 = du + len
          dv <- MUV.read dist v
          if d1 >= dv then return (H.Entry (-1) (-1)) else do
            MUV.write dist v d1
            return (H.Entry d1 v)
          )
        let queue2 = H.union queue1 $ H.fromList [e | e@(H.Entry p _) <- vds, p /= -1]
        loop graph dist queue2

{-
ghci> part12 "sample.txt" solve1
102
ghci> part12 "input.txt" solve1 
1128

一発通し。
-}

{-
part2
直進を4から10までだけ、に変えてやれと。
gだけ差し替えたらできるかな。というかijdか。
と思ったら、途中の1,2,3歩のコストを足し込むのが面倒だな。drop 3 か。
-}

solve2 ls = minimum
    [dv1 UV.! index bnds (h,w,False), dv1 UV.! index bnds (h,w,True)
    ,dv2 UV.! index bnds (h,w,False), dv2 UV.! index bnds (h,w,True)]
  where
    h = length ls
    w = length $ head ls
    marr = listArray ((1,1),(h,w)) $ map digitToInt $ concat ls
    bnds = ((1,1,False),(h,w,True))  -- 垂直、水平
    cnt = rangeSize bnds
    dv1 = dijkstraV cnt (g !) (index bnds (1,1,False))
    dv2 = dijkstraV cnt (g !) (index bnds (1,1,True))

    g = accumArray (flip (:)) [] (0, pred $ rangeSize bnds)
        [ (index bnds (i,j,d), (index bnds (i1,j1,not d), wt))
        | i <- [1..h], j <- [1..w], d <- [False, True]
        , ijs0 <- ijd i j d
        , let ijs = takeWhile (inRange ((1,1),(h,w))) ijs0
        , let wts = scanl1 (+) $ map (marr !) ijs
        , ((i1,j1), wt) <- drop 3 $ zip ijs wts
        ]

    ijd i j True  = [[(i+d,j) | d <- [1..10]],[(i-d,j) | d <- [1..10]]]
    ijd i j False = [[(i,j+d) | d <- [1..10]],[(i,j-d) | d <- [1..10]]]

{-
ghci> part12 "sample.txt" solve2
94
ghci> part12 "sample2.txt" solve2
71
ghci> part12 "input.txt" solve2  
1268

なんか意外といい感じにgeneralに構成できていたようだ。
-}
