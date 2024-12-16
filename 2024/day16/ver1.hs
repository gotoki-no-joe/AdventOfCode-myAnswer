{-
トナカイの現在のコスト、位置、向きを、コストの降順にキューにして幅優先探索すればいい。
全体をグラフで表現してダイクストラ法でも同じだけど、グラフ作るの大変でしょう。

訪問済みフラグは、やはり向きも込みでないといけない。
-}

import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import qualified Data.Heap as PQ

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test0 = runner "samp1.txt" part1
test1 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

dN, dE, dW, dS :: Int
dN = 0
dE = 1
dW = 2
dS = 3

walk (i,j) d =
  case d of
    0 -> (pred i, j)
    1 -> (i, succ j)
    2 -> (i, pred j)
    3 -> (succ i, j)

turn 0 = [1,2]
turn 1 = [0,3]
turn 2 = [0,3]
turn 3 = [1,2]

part1 :: UArray (Int,Int) Char -> Int
part1 arr = runST $
  do
    visited <- newArray ((l,0),(u,3)) False
    loop visited $ PQ.singleton $ PQ.Entry 0 (start, dE)
  where
    start = head [p | (p,'S') <- assocs arr]
    goal  = head [p | (p,'E') <- assocs arr]
    (l,u) = bounds arr

    loop :: STUArray s ((Int,Int),Int) Bool -> PQ.Heap (PQ.Entry Int ((Int,Int),Int)) -> ST s Int
    loop visited queue = do
      let Just (PQ.Entry cost (p,d), queue1) = PQ.viewMin queue
      if p == goal then return cost else do -- ゴールした
        v <- readArray visited (p, d)
        if v then loop visited queue1 else do -- 訪問済みの場所だった
          writeArray visited (p, d) True
          let qadd = PQ.fromList $ [PQ.Entry (succ cost) (p1,d) | let p1 = walk p d, arr ! p1 /= '#'] ++ [PQ.Entry (cost + 1000) (p,d1) | d1 <- turn d]
          loop visited $ PQ.union queue1 qadd

{-
パート2

しばらく意味がわからなかったけれど、
・コストが同じ、同着一位な経路がいくつかある
・そのいずれかに属するマスを全て数えよ
といってるのか。

コストはわかっているから、深さ優先探索でそのコストでゴールできる経路を全て数え上げる、
とやると、二手に分かれては合流するときにえらいことになる。

単純な「訪問済みフラグ」だと、同着で合流するエージェントも殺されてしまう。
同着で合流したとき、情報をマージする仕組みを入れる必要がある。

visitedの配列は、「そのマスに到着する最短時刻」とする。
maxBoundのときは未到達で、そのときは単純に書き込めばいい。
次に来たエージェントがコスト超過のときは、敗退する。
コスト同着のときは、マージするように、つまりマスには「そのエージェントの踏んできたマス集合」みたいなものを付ける。

キューにいれられるエージェント自身は、どこからどこへの移動か、だけ、frontierということだけ覚えておいて、
順番がきたときに、集積された visited の情報を持ち上げて次に移す、とやればいい。


じゃなくて、ダイクストラ法で全ての頂点について距離を求めて、
ゴールからスタートまで、最短経路に含まれるような頂点を抽出して、
向きに関してまとめてマスとして勘定する、だこれ。わはは。
最初の直観が正しかったか。
-}
