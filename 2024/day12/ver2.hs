import Data.Array

import Data.Array.ST
import Control.Monad.ST
import Control.Monad

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      garden = listArray ((1,1),(h,w)) $ concat ls
      ans = f garden
  print ans

test1 = runner "sample.txt" (part12 1)
main1 = runner "input.txt"  (part12 1)

part12 sw garden
  | sw == 1 = sum $ zipWith (*) (elems region) (elems peri)
  | sw == 2 = sum $ zipWith (*) (elems region) (elems edges)
  where
    -- 庭の座標系
    bnds@(_,(h,w)) = bounds garden
    -- 背番号の範囲
    ub = h * w - 1
    unites =
      [(index bnds (i,j), index bnds (i, succ j))  -- 右隣と文字が同じとき結合
      | i <- [1 .. h], j <- [1 .. pred w]
      , garden ! (i,j) == garden ! (i, succ j)] ++
      [(index bnds (i,j), index bnds (succ i, j))  -- 下隣と文字が同じとき結合
      | i <- [1 .. pred h], j <- [1 .. w]
      , garden ! (i,j) == garden ! (succ i, j)]
    -- Union-Findを実行し、getRootの結果を集めた配列を作る
    root :: Array (Int,Int) Int
    root = runSTArray $ do
      uf <- newUF (0, ub)
      forM_ unites (uncurry (uniteUF uf))
      rs <- forM [0 .. ub] (getRoot uf)
      newListArray bnds rs
    -- 面積
    region :: Array Int Int
    region = accumArray (+) 0 (0, ub) [(r,1) | p <- range bnds, let r = root ! p]
-- パート1
    -- 周長
    getg p
      | inRange bnds p = garden ! p
      | otherwise      = '#'
    peri :: Array Int Int
    peri = accumArray (+) 0 (0, ub)
        [ (r, 1)
        | p@(i,j) <- range bnds, let r = root ! p, let gp = garden ! p
        , q <- [(pred i,j),(succ i,j),(i, pred j),(i, succ j)]
        , gp /= getg q
        ]
-- パート2
    -- 角の個数
    countEdge (i,j) = length $ filter id
      [e2 && e4 && n1, n2 && n4  -- 左下、凹、凸
      ,e8 && e4 && n7, n8 && n4  -- 左上、凹、凸
      ,e2 && e6 && n3, n2 && n6  -- 右下、凹、凸
      ,e6 && e8 && n9, n6 && n8  -- 右上、凹、凸
      ]
      where
        gs = [getg (p,q) | p <- [pred i .. succ i], q <- [pred j .. succ j]]
        [_ ,e2,_ ,e4,_,e6,_ ,e8,_ ] = map (gs !! 4 ==) gs
        [n1,n2,n3,n4,_,n6,n7,n8,n9] = map (gs !! 4 /=) gs
    edges :: Array Int Int
    edges = accumArray (+) 0 (0, ub) [(root ! p, countEdge p) | p <- range bnds]

test2 = runner "sample.txt" (part12 2)
main2 = runner "input.txt"  (part12 2)

-- Union-Find

-- 自分の番号を指しているとき、自分が代表元
type UnionFind s = STUArray s Int Int

-- Union-Find構造体を作る
newUF :: (Int,Int) -> ST s (UnionFind s)
newUF bnds = newListArray bnds $ range bnds

-- 代表元を得る
getRoot :: UnionFind s -> Int -> ST s Int
getRoot uf i = loop i
  where
    loop j = do
      k <- readArray uf j
      if k == j then return j else do
        r <- loop k
        when (r /= k) $ writeArray uf j r
        return r

-- 統合する。
-- 元々同じ分割に属していたらfaを実行する
-- 統合が実際に行われたとき、元の代表元2つをペアにして返す（sndが統合後の代表元）
uniteUF :: UnionFind s -> Int -> Int -> ST s (Maybe (Int,Int))
uniteUF uf i j = do
  a <- getRoot uf i
  b <- getRoot uf j
  if a == b then return Nothing else do
    writeArray uf a b
    return $ Just (a,b)
