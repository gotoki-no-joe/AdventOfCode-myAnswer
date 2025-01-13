import Data.List
import qualified Data.Set as S

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe
import Data.Array

import Debug.Trace

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

-- 3文字固定っぽいけど、とりあえずそのまま読み込む。
parse :: String -> [String]
parse l = as : words bs
  where
    (as,_:_:bs) = break (':' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = graph0 -- ans
  where
-- 出現するノード名全てを持つ集合を使って、背番号を振る。
    nodes = S.fromList $ concat ls
    n = S.size nodes
-- 背番号0～で、辺の情報のリストにする。
    aijs = zip [0 ..] [(i,j) | u:vs <- ls, let i = S.findIndex u nodes, v <- vs, let j = S.findIndex v nodes]
    m = length aijs
-- 辺の重複なし。length ijs == length $ nub ijs

-- 後半の部分。与えられた辺リストを全て使って、1～nが連結かそうでないかをunion-findで判定する。
-- 分割数nから始めて、統合が起きるたびにdecrして、最後に1まで至ったかどうかで判断する。
    connected ijs = runST $ do
      uf <- newUF (0, pred n)
      cntM <- foldM (\cnt (i,j) -> do
        mab <- uniteUF uf i j
        return $ if isJust mab then pred cnt else cnt
        ) n ijs
      return $ cntM == 1

-- いずれか一つの辺を抜いた辺リストに対して、統合結果が2群になったとき Just (a,b) を返す
    sampleComp ijs = runST $ do
      uf <- newUF (0, pred n)
      cntM <- foldM (\cnt (i,j) -> do
        mab <- uniteUF uf i j
        return $ if isJust mab then pred cnt else cnt
        ) n ijs
      if cntM /= 2 then return Nothing else do
        arr <- newArray (0, pred n) 0 :: ST s (STUArray s Int Int)
        forM_ [0 .. pred n] $ \i -> do
          j <- getRoot uf i
          readArray arr j >>= writeArray arr j . succ
        ab <- filter (0 <) <$> getElems arr
        let a:b:_ = ab
        return $ Just (a,b)

-- サンプルも、3本抜くんだった。違ってた。
    sampleMain = loop [] $ map snd aijs
      where
        loop _ [] = []
        loop ijs1 (ij:ijs2) =
          case sampleComp (ijs1 ++ ijs2) of
            Just (a,b) -> (a, b, a * b) : loop (ij:ijs1) ijs2
            Nothing    -> loop (ij:ijs1) ijs2

-- 全て入ったグラフ
    graph0 :: Array Int [(Int,Int)]
    graph0 = accumArray (flip (:)) [] (0, pred n) $ concat [[(i, (j,a)), (j, (i,a))] | (a, (i, j)) <- aijs]
-- 全ての辺
    edges :: Array Int (Int,Int)
    edges = listArray (0, pred m) $ map snd aijs

    ans =
      [ (x, y, x * y)
      | (a, ij@(i,j)) <- aijs -- 辺aを抜く辺として選ぶ
      , let g1 = delEdge graph0 i j -- そういうグラフを作る
      , let p = findPath g1 i j  -- iからjの最短経路
      , b <- p -- その中の一辺を選び
      , a < b -- ただしaより後ろのものだけ
      , let (k,l) = edges ! b
      , let g2 = delEdge g1 k l -- その辺も抜いて
      , let p1 = findPath g2 i j
      , let p2 = findPath g2 k l
--      , traceShow (a, b, intersect p1 p2) True
      , c <- intersect p1 p2 -- 二つの経路に共通する辺を選んで
      , a < c
      , Just (x,y) <- [sampleComp $ map snd $ filter (\(i,_) -> notElem i [a,b,c]) aijs]
      ]

-- グラフから (i,j) な辺を除く
delEdge g i j = accum del g [(i,j),(j,i)]
  where
    del jas i = filter ((i /=) . fst) jas

-- グラフのiからjへの経路な辺リストを返す。必ずあるものとして。
findPath g i j = runST $
  do
    dist <- newArray (bounds g) Nothing :: ST s (STArray s Int (Maybe [Int]))
    writeArray dist i $ Just []
    loop dist [i] []
    fromJust <$> readArray dist j
  where
    loop _ist [] [] = return ()
    loop dist [] news = loop dist news []
    loop dist (i:is) news = do
      disti <- readArray dist i
      news1 <- foldM (\ns (j,a) -> do
        distj <- readArray dist j
        if isJust distj then return ns else do
          writeArray dist j $ (a :) <$> disti
          return $ j : ns
        ) news (g ! i)
      loop dist is news1



-- データ表現
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
-- 統合が実際に行われたとき、元の代表元2つをペアにして返す（sndが統合後の代表元）
uniteUF :: UnionFind s -> Int -> Int -> ST s (Maybe (Int,Int))
uniteUF uf i j = do
  a <- getRoot uf i
  b <- getRoot uf j
  if a == b then return Nothing else do
    writeArray uf a b
    return $ Just (a,b)

{-
part1 ls = (S.size nodes, length ijs)

ghci> test1
(15,33)
ghci> main1
(1435,3218)
-}

{-
無向グラフがある。
3本の辺を切り離すことで、連結部分グラフが二つになるようにしたい。

ってこれどうやって解くんだ。
sample.txtの1つなら、union-findでも使って、一人だけ仲間はずれを総当たりすればわかるが、
33辺^2は大したことない。

しかしinput.txtは1186行あって、辺は6本くらいあったりするので、
そこから3つを総当たりは無茶。
といっても、1000を越えるノードのある普通のグラフで、3本だけで分断されるというのはわりと特徴的な見た目なはず。

一つの頂点に目星を付けて、そこから全ての頂点への最短経路を作る。
そこに頻出する辺が、というのは、たるむかもしれないからダメだ。

頂点から頂点への距離と経路を調べて、
辺の両端の頂点からある頂点への距離と経路が、自分を戻ったものになる、で概ね分断される、は普通か。

頂点集合の方を総当たりで探して、そこをまたぐ辺が3本しかないような選び方を見つければよい。
しかし2^1000通りになるんだが。

ある辺(u1,v1)に疑いをかける。その辺を切ったグラフで、u1-v1間を結ぶ最短経路を取り出す。
その経路にある辺(u2,v2)が次の容疑者。一人選んで切る。
このグラフで u1-v1間を結ぶ最短経路と、u2-v2間を結ぶ最短経路に、共通する辺はいっぱいありそうだけど、
それらの中で、切ったらu1-v1,u2-v2, u3-v3間が連結でなくなり、2群に分ける結果となるものが答え。

最初の選択肢は辺の本数Mとおり、
しかし次は経路長なので、min(N-1,M-1)が最悪で、もっと小さいと考えられる。
なのでO(M^3)にはならずに答えを見つけられると思うのだけど。
最悪N-1やM-1になったとして、それを切って迂回する経路、つまりもっと長い経路はどこにもないので。
つまりmin(N,M)-2ってことか。
-}

{-
sampleが動かん。
hfx = 3, pzl = 9 == edge 17
bvb = 0, cmg = 1 == edge 10
nvd = 8, jqt = 4 == edge 2
ghci> test1
(fromList ["bvb","cmg","frs","hfx","jqt","lhk","lsr","ntq","nvd","pzl","qnr","rhn","rsh","rzs","xhk"],
[(0,(4,11)),(1,(4,14)),(2,(4,8)),(3,(12,2)),(4,(12,9)),(5,(12,6)),(6,(14,3)),(7,(1,10)),(8,(1,8)),(9,(1,5))
,(10,(1,0)),(11,(11,14)),(12,(11,0)),(13,(11,3)),(14,(0,14)),(15,(0,3)),(16,(9,6)),(17,(9,3)),(18,(9,8))
,(19,(10,8)),(20,(7,4)),(21,(7,3)),(22,(7,0)),(23,(7,14)),(24,(8,5)),(25,(6,5)),(26,(13,10)),(27,(13,1))
,(28,(13,6)),(29,(13,12)),(30,(2,10)),(31,(2,5)),(32,(2,6))],

いや、traceが邪魔して見えなかっただけか？

ghci> test1
[(6,9,54),(6,9,54)]
ghci> main1
[(714,721,514794)Interrupted.

b < c が条件として入れられないのはなんだかなだけど、とりあえず答えは出た。
a < c なら、サンプルはいけた。
頂点ごとの探索をParallelで並列化してみようか。
ついでに、無駄な情報を落としてコードをすっきりさせるver2へ移動。

(0,11,[])
(0,1,[20])
(1,11,[])
(2,18,[])
(2,17,[8,10]) <= !!! b < c な条件抜いてみようか。
(2,13,[])
(3,32,[])
(3,5,[])
(4,16,[18])
(4,5,[32,3])
(5,32,[])
(6,13,[15])
(6,11,[])
(7,19,[])
(7,8,[])
(8,19,[24,9])
(9,24,[])
(10,15,[12])
(10,17,[0,2])
(10,18,[])
(12,14,[15])
(17,18,[25,16])
(18,24,[])
(18,25,[])
(20,23,[6,21])
(21,22,[23])
(25,31,[9])
(25,32,[])
(26,27,[28])
(28,29,[])
(30,31,[])
(31,32,[])
[])

ghci> test1
...
(0,11,[])
(0,1,[20])
(1,11,[])
(2,18,[])
(2,17,[8,10])
[(6,9,54) <-----!!!OK!
(2,13,[])
(3,32,[])
(3,5,[])
(4,16,[18])
(4,5,[32,3])
(5,32,[])
(6,13,[15])
(6,11,[])
(7,19,[])
(7,8,[])
(8,19,[24,9])
(9,24,[])
(10,15,[12])
(10,17,[0,2])
,(6,9,54) <---!!!OK!
(10,18,[])
(12,14,[15])
(17,18,[25,16])
(18,24,[])
(18,25,[])
(20,23,[6,21])
(21,22,[23])
(25,31,[9])
(25,32,[])
(26,27,[28])
(28,29,[])
(30,31,[])
(31,32,[])
])
-}