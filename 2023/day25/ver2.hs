import Data.List
import qualified Data.Set as S
import Data.Array

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
-- import Data.Maybe

import Debug.Trace

import Control.Parallel.Strategies

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> [String]
parse l = as : words bs
  where
    (as,_:_:bs) = break (':' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

type Graph = Array Int [(Int,Int)] -- 頂点番号、隣接頂点の番号と辺の番号

part1 ls = ans -- concat pans
  where
-- 出現するノード名全てを持つ集合を使って、背番号を振る。
    nodes = S.fromList $ concat ls
    n = pred $ S.size nodes
-- 辺の情報のリスト
--    ijs = [(i,j) | i:js <- map (map (succ . flip S.findIndex nodes)) ls, j <- js]
    ijs = [(i,j) | l <- ls, let i:js = map (succ . flip S.findIndex nodes) l, j <- js]
    m = length ijs
    edges = listArray (1,m) ijs
-- 全ての辺を備えたグラフ
    graph0 :: Graph
    graph0 = accumArray (flip (:)) [] (1,n) $ concat [[(i,(j,a)),(j,(i,a))] | (a, (i,j)) <- assocs edges]
-- 直列計算
    ans =
      [ (x, y, x * y)
-- 辺aを1つ（順に）選び、aを除いたグラフg1を作り
      | a <- [1 .. m], let (i,j) = edges ! a, let g1 = delEdge graph0 i j
-- g1でのiからjへの経路を作り
      , let p1 = findPath g1 i j
-- 経路上の全ての辺b、ただしaより大きいものを順に一つ選び、bを除いたグラフg2を作り
      , b <- p1, a < b, let (k,l) = edges ! b, let g2 = delEdge g1 k l
-- g2でのiからj, kからlへの経路を作り
      , let p2 = findPath g2 i j, let p3 = findPath g2 k l
-- その共通する辺c、ただしaより大きいものを順に一つ選び、cも除いたグラフg3を作り
      , c <- intersect p2 p3, a < c, let (p,q) = edges ! c, let g3 = delEdge g2 p q
-- g3でiからjが到達不能なときの、
-- iから到達できる頂点の個数とjから到達できる頂点の個数が答えをなす
      , Just (x,y) <- [reachCnt g3 i j]
      ]
-- 並列計算
    pans = runEval $ parList rpar $
      [ [ (x, y, x * y)
        | let (i,j) = edges ! a, let g1 = delEdge graph0 i j
        , let p1 = findPath g1 i j
        , b <- p1, a < b, let (k,l) = edges ! b, let g2 = delEdge g1 k l
        , let p2 = findPath g2 i j, let p3 = findPath g2 k l
        , c <- intersect p2 p3, a < c, let (p,q) = edges ! c, let g3 = delEdge g2 p q
        , Just (x,y) <- [reachCnt g3 i j]
        ]
      | a <- [1 .. m]
      ]

-- グラフから (i,j) な辺を除く
delEdge :: Graph -> Int -> Int -> Graph
delEdge g i j = accum del g [(i,j),(j,i)]
  where
    del xs y = filter ((y /=) . fst) xs

-- グラフのiからjへの経路な頂点リストを返す。必ずあるものとして。
findPath :: Graph -> Int -> Int -> [Int]
findPath g i j = runST $
  do
    dist <- newArray (bounds g) noPath :: ST s (STArray s Int [Int])
    writeArray dist i []
    loop dist [i] []
    readArray dist j
  where
    noPath = [-1]
    loop _ist [] [] = return ()
    loop dist [] news = loop dist news []
    loop dist (i:is) news = do
      disti <- readArray dist i
      news1 <- foldM (\ns (j,a) -> do
        distj <- readArray dist j
        if distj /= noPath then return ns else do
          writeArray dist j $ a : disti
          return $ j : ns
        ) news (g ! i)
      loop dist is news1

-- グラフのiとjからそれぞれ到達可能な頂点の個数を返す。
-- しかしiを調べているうちにjも踏んでしまったらNothingを返す
reachCnt g i j = runST $
  do
    fld <- newArray (bounds g) False :: ST s (STArray s Int Bool)
    writeArray fld i True
    x <- iter fld i 0
    fj <- readArray fld j
    if fj then return Nothing else do
      writeArray fld j True
      y <- iter fld j 0
      return $ Just (x,y)
  where
    iter fld p cnt0 = succ <$>
      foldM (\cnt (q,_) -> do
        f <- readArray fld q
        if f then return cnt else do
          writeArray fld q True
          iter fld q cnt
        ) cnt0 (g ! p)

main = main1

-- コンパイルしたら並列化するまでもなく一瞬で終わる。
-- どうしてインタプリタだと、続きを探し続けるんだ？
-- STArrayがghciだと遅いとかが関係している？
