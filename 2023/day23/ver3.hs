{-
サンプルもinput.txtも、実は、一方通行の壁で囲まれたマスにしか分岐がなくて、
単なる有向グラフの最長経路を探せと言っているだけなのでは？
-}

import Data.Array.Unboxed
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Bits

import Debug.Trace

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test12 = runner "sample.txt" part12

main12 = runner "input.txt" part12
main = main12

neighbors (i,j) = [(pred i,j), (succ i,j),(i, pred j),(i, succ j)]

part12 ls = (distG ! 2, dists2, maximum $ map snd dists2)
  where
    h = length ls
    w = length $ head ls
    bnds = ((0,1),(succ h,w))
    wall = replicate w '#'
-- 上と下に番兵を並べた以外はオリジナルの迷路
    fld0 = listArray bnds $ concat $ wall : ls ++ [wall] :: UArray (Int,Int) Char

-- 上下左右に矢印が2つ以上ある、交差点の座標、出発地点と目標地点も追加
    crosspoints = (1,2) : (h,pred w) :
      [ ij
      | (ij,'.') <- assocs fld0
      , 1 < length (filter (flip elem "^>v<" . (fld0 !)) $ neighbors ij )]
    cpN = length crosspoints
-- 交差点の座標から背番号 リスト順なのでスタートは1,ゴールは2
    cpm = M.fromList $ zip crosspoints [1 ..]
-- 交差点を+でマークした地図
-- さらに、出発地点すぐ下と目標地点すぐ上に `v` を置いて交差点に見せかける
    fld = fld0 // (((2,2),'v') : ((pred h, pred w),'v') : [(p,'+') | p <- crosspoints])

-- 指定された交差点の番号から、4方向のうち移動可能な方に出発し、隣の交差点までの距離とその交差点番号を組で返す
-- いつもの有向グラフ形式
    g = listArray (1, cpN) $ map distF crosspoints :: Array Int [(Int, Int)]
    distF p = [dist 1 p q | (c, q) <- zip "^v<>" $ neighbors p, fld ! q == c]
    dist d p q
      | fld ! q == '+' = (cpm M.! q, d)
      | otherwise = head [dist (succ d) q r | r <- neighbors q, r /= p, fld ! r /= '#']

-- そして集めるDP
-- グラフは結局一方通行なので、交差点の最も遠い距離は
-- 入ってくる辺の中で最も遠いもの。なのでgの逆を作ってから、それらからの距離の最大値を求める
    revg = accumArray (flip (:)) [] (1, cpN) [(q,(p,d)) | (p,qds) <- assocs g, (q,d) <- qds] :: Array Int [(Int,Int)]
    distG = listArray (1, cpN) $ map distGF $ elems revg :: Array Int Int
    distGF pds = maximum $ 0 : [d + distG ! p | (p,d) <- pds]

--  無向にしたグラフ
    gg = listArray (1, cpN) $ zipWith (++) (elems g) (elems revg) :: Array Int [(Int,Int)]
-- 各頂点に対して
-- oarr (スタートから訪問した頂点集合、最長距離）
-- oagent 登録されたばかりのoarrの要素
-- darr (ゴールから訪問した頂点集合、最長距離)
-- dagent 最新の要素
-- という4要素を持って、残り歩ける歩数のカウントも持って、
-- oagentの要素についてdarrを調べ、訪問頂点に被りがないものについて、距離の和が一つの経路、を出力
-- oagentからの移動先を求め、それらをoarrに追加し、oagentの次の値とする
-- 残り歩数カウントを1減らす
-- まだ残っているなら、攻守交代してループ
-- という手順で、半分全探索による経路の総当たりができるはず。
    oarr0 = listArray (1,cpN) $ IM.singleton (bit 1) 0 : repeat IM.empty
    darr0 = listArray (1,cpN) $ IM.empty : IM.singleton (bit 2) 0 : repeat IM.empty
    dists2 = loop cpN oarr0 oarr0 darr0 darr0
    loop :: Int -> Array Int (IM.IntMap Int) -> Array Int (IM.IntMap Int)
                -> Array Int (IM.IntMap Int) -> Array Int (IM.IntMap Int) -> [(Int,Int)]
    loop cnt _ _ _ _ | cnt <= 0 = [] -- おしまい。
    loop cnt _ _ _ _ | traceShow cnt False = error ""
    loop cnt oarr oagents darr dagents = (cpN - cnt, maximum res) : loop (pred cnt) darr dagents oarr1 oagents1
      where
        res = 0 : [dp + dq | (p, vpdp) <- assocs oagents, (vp, dp) <- IM.assocs vpdp, (vq, dq) <- IM.assocs $ darr ! p, vp .&. vq == bit p]
        oagents1 = accumArray (IM.unionWith max) IM.empty (1,cpN)
            [ (q, IM.singleton vq dpd)
            | (p, vpdp) <- assocs oagents, (vp, dp) <- IM.assocs vpdp, (q, dd) <- gg ! p, not $ testBit vp q
            , let vq = setBit vp q, let dpd = dp + dd
            , IM.findWithDefault 0 vq (oarr ! q) < dpd ]
        oarr1 = accum (IM.unionWith max) oarr $ assocs oagents1

{-
product [1 .. 17] = 355687428096000

これもでかいけど。

スタートから出発して、cpN / 2 ステップで到達できる先の頂点について、
通過済みの頂点集合ごとに最大の距離を集める。

ゴールから出発して、cpN / 2 ステップで到達できる先の頂点について、
通過済みの頂点集合が重複しない結果について、最大値と足し合わせる
…これ「全ての頂点を確かに通れる」ということ前提で考えてるな。
そうでないことも考えると、両方から出発して、交互に進めるincrementalなアルゴリズムでする？
両方合わせたステップ数がcpNに達したら、それ以上進めて合流するものがあるはずない、で止められる。

どうやって実装する？
arr = listArray (1,cpN) $ repeat []
agents = [(start,{start},0), (goal,{goal},0)]
から開始して、各エージェント (p,vis,d) について、
arr ! p ∈ (vis1, d1) で disjoint vis vis1 なものがあれば、d + d1 は一つの経路の長さ。出力する
arr ! p に (vis, d) を追記する、というか、visをキーにdの最大値を残す感じ。元々もっと大きいのがあったら何もしなくていい。
pの隣接頂点qでvisに含まれていないものについて、(q,vis+q,d+d) を次に調べる予定リストに入れる
特別な要素「カウントダウン」をキューに仕込めば、終了タイミングはわかる。
最後の１周はキューに次の要素を入れる必要もないので、「ウノ」フラグも付けておくといいかもしれない。

さらに、visをIntSetでなくbitmapで表現することで、「visをキーにdの最大値」がIntMapでできて便利かな？
そういう場合は？起こりうる。そして、それは同じタイミングで出てくるものなので、
次の周回に入る前にインターセプトするべき。キューに放り込んでしまうと手遅れなのだけど、
それを遅らせるのは大変に困難。むしろ、arrをスキャンして、popCountが特定の値のものを起点にする、でした方が楽なくらい。
（本当にこれをやると、不要な要素をスキャンしまくるからできない。）
その代わりに、新規なものだけ入れたarrDiffを作って、終わってからmergeする感じでやればいいのか。
それは、キューでない、arrと相似な形でnextを管理すると言っているだけだな。

offence/diffence交代制でやれば、スタートはゴールからのエージェントだけを調べることができて、
回数が尽きたらきっちり終われる。これか。

(2030,[(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,0)
,(11,2006),(12,2030),(13,2450),(14,2498),(15,2970),(16,2970),(17,3290)
,(18,3290),(19,3866),(20,4038),(21,4166),(22,4382),(23,4482),(24,4934)
,(25,4934),(26,5278),(27,5278),(28,5526),(29,5526),(30,5870),(31,5870)
,(32,6110),(33,6110),(34,6390),(35,6390)],6390)

traceShowでカウントダウンを観察できるようにしたら、コンパイル実行でも待てて、
徐々に遅くなっていったけどまぁ3分もかからずに答えが出た。
インタプリタなら遅延評価でdist2の内容がリアルタイムで観察できるのだけど、
コンパイルするとバッファされちゃうのか。

シーケンシャルな計算であることがすごく前提になっているので、Parallelを導入する隙がない。
難しいもんだ。

訪問済み頂点の包含関係に関して単調性があっていい。
訪問頂点が少ないのに距離が長いものがあるとき、ヘボいものを登録する必要がない。
それを探すのも時間かかるな。
-}
