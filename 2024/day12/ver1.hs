{-
連結性なら、ちまちまやらずにUnion-Findか。bfsでもできるけど。
-}

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed

import Data.Graph

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = sum $ zipWith (*) (elems region) (elems fence)
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    lb = minimum $ map (index bnds) $ range bnds
    ub = maximum $ map (index bnds) $ range bnds
    garden = listArray bnds $ concat ls :: UArray (Int,Int) Char
    unites =
      [(index bnds (i,j), index bnds (i, succ j)) | i <- [1 .. h], j <- [1 .. pred w], garden ! (i,j) == garden ! (i, succ j)] ++
      [(index bnds (i,j), index bnds (succ i, j)) | i <- [1 .. pred h], j <- [1 .. w], garden ! (i,j) == garden ! (succ i, j)]
    uf :: UArray (Int,Int) Int
    uf = listArray bnds $ runST $ do
      uf <- newUF (lb, ub)
      forM_ unites (\(i,j) -> uniteUF uf i j (return ()) (\_ _ -> return ()))
      forM [lb .. ub] (getRoot uf)
    region, fence :: UArray Int Int
    region = accumArray (+) 0 (lb,ub) [(r,1) | p <- range bnds, let r = uf ! p]
--    neighbors (i,j) = filter (inRange bnds) [(pred i,j),(succ i,j),(i, pred j),(i, succ j)]
    neighbors (i,j) = [(pred i,j),(succ i,j),(i, pred j),(i, succ j)]
    getg p
      | inRange bnds p = garden ! p
      | otherwise      = '#'
    fence = accumArray (+) 0 (lb,ub)
        [(r, length $ filter ((gp /=) . getg) $ neighbors p ) | p <- range bnds, let r = uf ! p, let gp = garden ! p]

-- データ表現
-- 自分の番号を指しているとき、自分が代表元
type UnionFind s = STUArray s Int Int

-- Union-Find構造体を作る
newUF :: (Int,Int) -> ST s (UnionFind s)
newUF bnds = newListArray bnds $ range bnds

-- 代表元を得る
getRoot :: UnionFind s -> Int -> ST s Int
getRoot uf i =
  do
    loop <- fixST $ \loop -> return $ \j -> do
      k <- readArray uf j
      if k == j then return j else do
        r <- loop k
        when (r /= k) $ writeArray uf j r
        return r
    loop i

-- 統合する。
-- 元々同じ分割に属していたらfaを実行する
-- 統合が実際に行われたとき、代表元2つを引数にtaを実行する（後ろが統合後の代表元）
uniteUF :: UnionFind s -> Int -> Int -> ST s a -> (Int -> Int -> ST s a) -> ST s a
uniteUF uf i j fa ta = do
  a <- getRoot uf i
  b <- getRoot uf j
  if a == b then fa else do
    writeArray uf a b
    ta a b

{-
パート2、どこかで見た気がするけどどうやったか思い出せない。

境界を見て、親の座標と、それを左手にみた向き、の組を集める。
を、接続しているものどうしをunifyするのがめんどくさいな。
座標点からの有向グラフで？
向きごとに別々に4回、フェンスを辺にしたグラフを作って、連結成分の個数を数えるか。

座標 (i,j) のマスについて、左上の格子点を格子点の (i,j) とする。
つまり、格子点は ((1,1),(h+1,w+1)) だけある。
(i,j) のマスと隣接する上下左右について、隣と色が違うときにフェンスを張ることになるが、
反時計回りに自分を囲むものと考えて、あるいは、左手に自分を掴みながら一周する向きで考えて
上：(i,j+1) -> (i,j)
下：(i+1,j) -> (i+1,j+1)
右：(i+1,j+1) -> (i,j+1)
左：(i,j) -> (i+1,j)
を接続する。まずはこれらをrootに集めよう。また、これらを向き番号0～3に割り振る。
-}

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2x ls = sum $ zipWith (*) (elems region) fence
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    lb = minimum $ map (index bnds) $ range bnds
    ub = maximum $ map (index bnds) $ range bnds
    garden = listArray bnds $ concat ls :: UArray (Int,Int) Char
    unites =
      [(index bnds (i,j), index bnds (i, succ j)) | i <- [1 .. h], j <- [1 .. pred w], garden ! (i,j) == garden ! (i, succ j)] ++
      [(index bnds (i,j), index bnds (succ i, j)) | i <- [1 .. pred h], j <- [1 .. w], garden ! (i,j) == garden ! (succ i, j)]
    uf :: UArray (Int,Int) Int
    uf = listArray bnds $ runST $ do
      uf <- newUF (lb, ub)
      forM_ unites (\(i,j) -> uniteUF uf i j (return ()) (\_ _ -> return ()))
      forM [lb .. ub] (getRoot uf)
    region :: UArray Int Int
    region = accumArray (+) 0 (lb,ub) [(r,1) | p <- range bnds, let r = uf ! p]
    neighbors (i,j) = [(pred i,j),(succ i,j),(i, pred j),(i, succ j)]
    getg p
      | inRange bnds p = garden ! p
      | otherwise      = '#'
    fence0 :: Array (Int,Int) [((Int,Int),(Int,Int))]
    fence0 = accumArray (flip (:)) [] ((lb,0),(ub,3))
        [ ((r, d), e)
        | p@(i,j) <- range bnds, let r = uf ! p, let gp = garden ! p
        , let qs = [(pred i,j),(succ i,j),(i,succ j),(i,pred j)]
        , let es = [((i,j+1),(i,j)), ((i+1,j),(i+1,j+1)), ((i+1,j+1),(i,j+1)), ((i,j),(i+1,j))]
        , (d,q,e) <- zip3 [0 .. 3] qs es
        , gp /= getg q
        ]
    fbnds = ((1,1),(succ h, succ w))
    countBulk es = length $ filter ((1 <) . length) $ components g
      where
        g = buildG (index fbnds (1,1), index fbnds (succ h, succ w))
            [(index fbnds a, index fbnds b) | (a,b) <- es]
    fence = [sum [countBulk $ fence0 ! (p,d) | d <- [0 .. 3]] | p <- [lb .. ub]]

{-
作戦あってるはずなんだけど、なんかバグる。part1と同じ数字がでてきちゃう ><
もっと簡単なアプローチとして「角を数える」があったわ。
電卓の5が自分として、2と4の両方が色違いのとき、凸角が右下に1 (3は不問)
2と4が同じ色で、3が色違いのとき、凹角が右下に1
を4隅全てに考えよ、という。
-}

part2 ls = sum $ zipWith (*) (elems region) (elems fence)
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    lb = minimum $ map (index bnds) $ range bnds
    ub = maximum $ map (index bnds) $ range bnds
    garden = listArray bnds $ concat ls :: UArray (Int,Int) Char
    unites =
      [(index bnds (i,j), index bnds (i, succ j)) | i <- [1 .. h], j <- [1 .. pred w], garden ! (i,j) == garden ! (i, succ j)] ++
      [(index bnds (i,j), index bnds (succ i, j)) | i <- [1 .. pred h], j <- [1 .. w], garden ! (i,j) == garden ! (succ i, j)]
    uf :: UArray (Int,Int) Int
    uf = listArray bnds $ runST $ do
      uf <- newUF (lb, ub)
      forM_ unites (\(i,j) -> uniteUF uf i j (return ()) (\_ _ -> return ()))
      forM [lb .. ub] (getRoot uf)
    region, fence :: UArray Int Int
    region = accumArray (+) 0 (lb,ub) [(r,1) | p <- range bnds, let r = uf ! p]
    getg p
      | inRange bnds p = garden ! p
      | otherwise      = '#'
    countEdge (i,j) =
      (if e2 && e4 && n1 then 1 else 0) + -- 左下、凹
      (if n2 && n4       then 1 else 0) + -- 左下、凸
      (if e8 && e4 && n7 then 1 else 0) + -- 左上、凹
      (if n8 && n4       then 1 else 0) + -- 左上、凸
      (if e2 && e6 && n3 then 1 else 0) + -- 右下、凹
      (if n2 && n6       then 1 else 0) + -- 右下、凸
      (if e6 && e8 && n9 then 1 else 0) + -- 右上、凹
      (if n6 && n8       then 1 else 0)   -- 右上、凸
      where
        gs = [getg (p,q) | p <- [pred i .. succ i], q <- [pred j .. succ j]]
        [e1,e2,e3,e4,_,e6,e7,e8,e9] = map (gs !! 4 ==) gs
        [n1,n2,n3,n4,_,n6,n7,n8,n9] = map (gs !! 4 /=) gs
    fence = accumArray (+) 0 (lb,ub) [(uf ! p, countEdge p) | p <- range bnds]

{-
ghci> test1
1930
ghci> main1
1387004
ghci> test2
1206
ghci> main2
844198

角を数える作戦はとりあえず成功したけど、countEdgesはちょっとかっこ悪いな。もうちょっと何とかならんかったんか。
そして part2x の正攻法、グラフにして島を数えるやつ…わかった。
pred length では辺を数えているから、length of (1 <) . length をしないといかんのだった。わかった。

うん、本番データでやるとめちゃめちゃ重い。何をしたかな。
あと、上下左右を全て分けなくても、縦と横だけで十分だ。それらは間違って接続したりしない。

すごいかかったけど、正解はした。
ghci> runner "sample.txt" part2x
1206
ghci> runner "input.txt" part2x 
844198
縦横統合したら時間半分には…ならんし、なっても遅いわ。そんなもんか。
ていうか統合したら答え違う。ん？
ghci> runner "sample.txt" part2x
1206
ghci> runner "input.txt" part2x
836294

そうだ、チェッカー模様に騙されるんだこれ。componentsでなくdffならいいのかな？
いやこれはこれで違う（頂点はそれぞれ一度しか出てこない）から、統合は無理で、グラフを使うのは重くてダメって結論だね。
-}
