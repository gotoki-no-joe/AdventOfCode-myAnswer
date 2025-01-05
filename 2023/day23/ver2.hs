{-
サンプルもinput.txtも、実は、一方通行の壁で囲まれたマスにしか分岐がなくて、
単なる有向グラフの最長経路を探せと言っているだけなのでは？
-}

import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S

--import Debug.Trace

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test12 = runner "sample.txt" part12

main12 = runner "input.txt" part12

checkBranch ls =
  [ (i,j)
  | ((i,j), '.') <- assocs fld, i > 1, i < h, all (('.' ==) . (fld !)) [(pred i,j), (succ i,j),(i, pred j),(i, succ j)]]
  where
    h = length ls
    w = length $ head ls
    fld = listArray ((1,1),(h,w)) $ concat ls :: UArray (Int,Int) Char

{-
追加で、と考えるから面倒なので、最初からそういうつもりでやりなおそう。
深さ優先探索で探索し、そのとき、今どこが出発地点かを覚えておき、
「xxからxxへの有向辺の長さはxx」という情報を集約する。
DFSというか、マップをスキャンしての一方通行で再帰的にでなくやる感じだ。
-}

-- やりやすいように、周囲に矢印が複数ある分岐マスの記号を変えよう。

part12 ls = (distG ! 2, ans2)
  where
    h = length ls
    w = length $ head ls
    bnds = ((0,1),(succ h,w))
    wall = replicate w '#'
-- 上と下に番兵を並べた以外はオリジナルの迷路
    fld0 = listArray bnds $ concat $ wall : ls ++ [wall] :: UArray (Int,Int) Char

-- 上下左右に矢印が2つ以上ある、交差点の座標、出発地点と目標地点も追加
    crosspoints = (1,2) : (h,pred w) :
      [ (i,j)
      | ((i,j),'.') <- assocs fld0
      , 1 < length (filter (flip elem "^>v<" . (fld0 !)) [(pred i,j), (succ i,j),(i, pred j),(i, succ j)])]
    cpN = length crosspoints
-- 交差点の座標から背番号 リスト順なのでスタートは1,ゴールは2
    cpm = M.fromList $ zip crosspoints [1 ..]
-- 交差点を+でマークした地図
-- さらに、出発地点すぐ下と目標地点すぐ上に `v` を置いて交差点に見せかける
    fld = fld0 // (((2,2),'v') : ((pred h, pred w),'v') : [(p,'+') | p <- crosspoints])

-- 指定された交差点の番号から、4方向のうち移動可能な方に出発し、隣の交差点までの距離とその交差点番号を組で返す
-- いつもの有向グラフ形式
    g = listArray (1, cpN) $ map distF crosspoints :: Array Int [(Int, Int)]
    distF p@(i,j) =
      [dist 1 p q | let q = (pred i,j), fld ! q == '^'] ++
      [dist 1 p q | let q = (succ i,j), fld ! q == 'v'] ++
      [dist 1 p q | let q = (i,pred j), fld ! q == '<'] ++
      [dist 1 p q | let q = (i,succ j), fld ! q == '>']
    dist d p q@(i,j)
      | fld ! q == '+' = (cpm M.! q, d)
      | otherwise = head [dist (succ d) q r | r <- [(pred i,j), (succ i,j),(i, pred j),(i, succ j)], r /= p, fld ! r /= '#']

-- そして集めるDP
-- グラフは結局一方通行なので、交差点の最も遠い距離は
-- 入ってくる辺の中で最も遠いもの。なのでgの逆を作ってから、それらからの距離の最大値を求める
    revg = accumArray (flip (:)) [] (1, cpN) [(q,(p,d)) | (p,qds) <- assocs g, (q,d) <- qds] :: Array Int [(Int,Int)]
    distG = listArray (1, cpN) $ map distGF $ elems revg :: Array Int Int
    distGF pds = maximum $ 0 : [d + distG ! p | (p,d) <- pds]

-- と、安心させておいて、本番は後半だったか。
--  グラフを無向にして、dfsで総当たりする感じでしょう。到達済みの頂点は記録しておいて再訪禁止で。
    gg = listArray (1, cpN) $ zipWith (++) (elems g) (elems revg) :: Array Int [(Int,Int)]
{-
    ans2 = maximum $ iter [1] 0 1 []
    iter visited dist p rest
      | p == 2 = dist : rest
      | otherwise = foldr step rest (gg ! p)
      where
        step (q, d) rest
          | elem q visited = rest
          | otherwise = iter (q : visited) (d + dist) q rest
-}
    ans2_naive = iter_naive (IS.singleton 1) 1
    iter_naive _ 2 = 0
    iter_naive visited p = maximum $ -1 :
      [d + iter_naive (IS.insert q visited) q | (q,d) <- gg ! p, IS.notMember q visited]

    ans2 = memoizeMap iter (IS.singleton 1, 1)
    iter _nner (_isited, 2) = ([], 0)
    iter inner (visited, p) = (args, res)
      where
        args = [(IS.insert q visited, q) | (q,d) <- gg ! p, IS.notMember q visited]
        res = maximum $ -1 : [d + inner (IS.insert q visited, q) | (q,d) <- gg ! p, IS.notMember q visited]

{-
ghci> test12
(94,154)
ghci> main12
(2030,Interrupted.

naiveな方法では計算爆発するなら、メモ化に逃げるかね。visitedを正規化するのを忘れずに。
UArray Int Boolでも可能だけど、どうしよう。まぁIntSetでいいか。

ていうか、これメモ化で再利用できる情報あるんだろうか。むしろ足を引っ張ってるだけなのでは？
可能な分岐は最大で3なので、大したことにはならないかとタカをくくったのだが、
34! = 295232799039604140847618609643520000000 を調べ上げることにはならない、と。
しかしこの刺さりようは何だろう。

順列組み合わせを作るフリをして、到達不能な要素は後回しにして、最後に2に到達する経路だけ考えて、
というこの最後がトラップなのかも。変なところで行き詰まる経路も全て調べているから。
ということは、半分全探索の出番か？

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
-}

memoizeMap :: Ord t => ((t -> s) -> t -> ([t], s)) -> t -> s
memoizeMap fya x = m M.! x
  where
    m = loop M.empty (S.singleton x)
    loop old new
      | S.null new = old
      | otherwise  = loop old1 new1
      where
        (kvs, jss) = unzip [((k,v),js) | k <- S.elems new, let (js, v) = fya (m M.!) k]
        old1 = M.union old $ M.fromList kvs
        new1 = S.fromList $ concatMap (filter (flip M.notMember old1)) jss
