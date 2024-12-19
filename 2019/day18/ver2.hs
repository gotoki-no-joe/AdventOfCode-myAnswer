{-# LANGUAGE PartialTypeSignatures #-}

{-
2019はあとここだけなので、何とかしたい。
地図は82x82 アルファベットはA～Z全て出現している。
どれをとってどれをとってないか、という意味では2^26 = 67,108,864通りの場合がある。
多いですな。

以前のコード ver1 は、マップが変化することを気にして、
- 現在の位置から到達可能な全ての鍵とその距離をマップ上で数える
- その全ての方法を探索
という感じなのだと思う。マップ上で1マスずつ進む計算が繰り返されて重い。

鍵と扉と初期位置をノードとし、床だけを辿って到達できる間を無向辺でつないだグラフを作っておく。
自分は、初期位置または今確保したいずれかの鍵の位置から次の探索をする。

ダイクストラ法で探索をするとき、確保していない鍵への距離が知りたい。
確保していない鍵の扉は通れないので、グラフ関数で距離を聞かれたときに、辺はないとうそぶく。
か、確保していない鍵の扉からは出る辺のない、閉じ込めで解決できる。

そして目標は「全ての鍵を確保する最短経路」なのだけど、
深さ優先探索で全探索するのはつらいので、幅優先探索というか、
トリップ距離をキーとする優先度で探索するのは、ダイクストラ法に他ならない！
全ノードの数はそれこそ 2^26 になるので、そういう実装にはならないけど。
キューの状態は、確保済みのキーの集合と現在位置でいいので、メモリにもやさしい！
-}

import Data.Array.Unboxed

import qualified Data.Map as M

import Data.Array.ST
import Control.Monad
import Control.Monad.ST

import qualified Data.Heap as H

import Data.Char
import Data.List
import Data.Maybe

import Data.Bits
import qualified Data.IntSet as IS

import Debug.Trace

import System.CPUTime

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls :: UArray POS Char
  print $ f fld

t1 = runner "s1.txt" part1 >> print "8 ab"
t2 = runner "s2.txt" part1 >> print "86 abcde"
t3 = runner "s3.txt" part1 >> print "132 bacdfeg"
t4 = runner "s4.txt" part1 >> print "136 afbjgnhdloepcikm or else"
t5 = runner "s5.txt" part1 >> print "81 acfidgbeh or else"
main1 = runner "input.txt" part1

main = do
  t0 <- getCPUTime
  t4
  t1 <- getCPUTime
  print $ t1 - t0
  main1
  t2 <- getCPUTime
  print $ t2 - t1

{-
コンパイル実行するつもりでこれを書いているうちに終わった。

ghci> t1
(8,"ab")
"8 ab"
ghci> t2
(86,"abdcef")
"86 abcde"
ghci> t3
(132,"bdcafeg")
"132 bacdfeg"
ghci> t4
(136,"bngecifdlhmakjop")
"136 afbjgnhdloepcikm or else"
ghci> t5
(81,"acifgdbeh")
"81 acfidgbeh or else"
ghci> main1
(4668,"wiyjdrosqtleupznxgbfhkcvam")
ghci>
-}

type POS = (Int,Int)

part1 :: UArray POS Char -> _ -- (Int, String)
part1 fld = res
  where
-- フィールド上で相互の距離を計測する
    distMap = computeDist fld
-- ダイクストラ法のために鍵の個数を数える。番号を振るため。大したことないから常に-26～+26でするか？
--    numKeys = length $ filter isLower $ elems fld

-- findPaths distMap 現在位置 実取得の鍵リスト
-- ができるようになったので、中核の探索部を書こう。
-- 鍵は一つも持っていない、位置 @ をキューに入れて始めて、
-- キューの先頭が、鍵を全て手に入れた、になったとき、それを返す
-- キューが空になることはないだろう。
-- キューの先頭の設定で findPathsを実行して、
-- 結果で得られる、次に取得できる鍵を取得して、歩数を進んだ状態をキューに追加する。
-- って、それが「取得した順序」になるように、「取得済みの鍵」でやるべき？
-- それだと、完了が調べにくいのさ。順序を諦めると楽だが。二本立てにするか。

    res = searchLoop IS.empty $ H.singleton $ H.Entry 0 ('@', filter isLower $ elems fld, "")

    searchLoop visited queue
      | null notyet = (d, reverse got) -- 発見！
      | IS.member statNum visited = searchLoop visited queue1 -- 出遅れた
      | otherwise   = searchLoop (IS.insert statNum visited) $ H.union queue1 $ H.fromList es
      where
        Just (ent@(H.Entry d (pos, notyet, got)), queue1) = H.viewMin queue
        statNum = encode pos notyet
        es = [H.Entry (d + dist) (nextkey, delete nextkey notyet, nextkey : got) | (nextkey, dist) <- findPaths distMap pos notyet]

-- 調査済みの状態をIntSetに記録する。STUArray (0-27,Bool,...,Bool) Bool 固定でもできなくはないけどさ。
-- pos と notyet を整数にエンコードするには、
-- posに掛けるための 2^m 鍵の数なベースが必要。
--    encodeBase = bit $ length $ filter isLower $ elems fld
-- いやもうそのbaseは 2^26 固定でいいや。
-- notyetの要素を押し込むのも簡単だ。
    encode pos notyet = sum $ char2num pos * 2^26 : map (bit . pred . char2num) notyet

members = ['Z','Y' .. 'A'] ++ '@' : ['a' .. 'z']
-- 番号と文字の相互変換は必要だ。
num2char k = members !! (k + 26)
char2num c = fromJust (elemIndex c members) - 26

{-
@ = 0
a - z : 1 ～ 26
A - z : -1 ～ -26
と番号を付けて、ダイクストラ法の頂点にするか。
まだ踏んでない鍵のフラグを、ビット集合で管理する番号にもなる。

まず、フィールドを読み込んで、各頂点の位置を見つけ、あるやつがどれかも見つけ、
相互の到達性と距離を数える。
-}

computeDist :: UArray POS Char -> M.Map Char [(Char, Int)]
computeDist fld = M.mapWithKey (\k _ -> bfs k) c2ij
  where
    c2ij = M.fromList [(c,ij) | (ij, c) <- assocs fld, notElem c ".#"]

-- 指定された文字を始点として、到達できる他の文字への最短距離をbfsで見つける
    bfs c = runST $ do
      visited <- newArray (bounds fld) False :: ST s (STUArray s POS Bool)
      let cp = c2ij M.! c
      writeArray visited cp True
      loop <- fixST $ \loop -> return $ \ans cnt ps0 news -> do
        case ps0 of
          [] | null news -> return ans -- おしまい
             | otherwise -> loop ans (succ cnt) news [] -- ピンポン
          (p@(i,j):ps) -> do
            case fld ! p of
-- 現在位置の文字が # なら、壁にめり込んだので進まないで終了
              '#' -> loop ans cnt ps news
-- 現在位置の文字が . c 以外なら、相手を見つけたので距離をansに追加する
-- このとき、それ以上は進まないので、子を作らずに終了
              cp | notElem cp [c,'.'] -> loop ((cp,cnt):ans) cnt ps news
-- . c のときは、上下左右に広がる
                 | otherwise -> do
                     news1 <- foldM (\news q -> do
                                b <- readArray visited q
                                if b then return news else do
                                  writeArray visited q True
                                  return (q : news)
                                ) news [(pred i,j),(succ i,j),(i,pred j),(i,succ j)]
                     loop ans cnt ps news1
      loop [] 0 [c2ij M.! c] []

{-
各ノードからの距離が測れたので、グラフとしてダイクストラ法を実行できる。
どんな設定でするのかというと、
@の現在位置と、確保**できていない**鍵のリストが変わるバリエーションになる。
その結果、edgeが、確保できていない鍵の扉からは出なくなる。
そしてそこから取り出したいのは、確保できていない鍵のうち、到達できるものとそこまでの距離。
それらを使って上位のダイクストラ法を回す予定。

上のbfsもダイクストラ法で置き換えた方がコードが短くていいかもな。
-}

-- @gotoki_no_joe
dijkstra :: (Int, Int)           -- 頂点番号の範囲
         -> (Int -> [(Int,Int)]) -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                  -- 開始点
         -> ST s (STUArray s Int Int)
dijkstra bnds edges start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    loop <- fixST $ \loop -> return $ \queue ->
      if H.null queue then return dist else do
        let Just (H.Entry cost u, queue1) = H.uncons queue
        du <- readArray dist u
        if du < cost then loop queue1 else do
          queue2 <- foldM (\q (v, we) -> do
            let duv = du + we
            dv <- readArray dist v
            if dv <= duv then return q else do
              writeArray dist v duv
              return $ H.insert (H.Entry duv v) q
            ) queue1 (edges u)
          loop queue2
    loop $ H.singleton (H.Entry 0 start)

-- distMap : computeDist の結果
-- c : マップで現在自分のいる位置の文字
-- notyets : まだ取得していない鍵、空ではない
-- 結果 : 到達できる鍵について、現在位置からそこまでの距離
findPaths :: M.Map Char [(Char, Int)] -> Char -> String -> _ -- [(Char, Int)]
findPaths distMap c notyets = runST $
  do
    dists <- dijkstra (-26, 26) edges (char2num c)
    foldM (\res d -> do
      l <- readArray dists $ char2num d
      return $ if l == maxBound then res else (d, l) : res
      ) [] notyets
  where
    closedDoors = map (negate . char2num) notyets
    edges i
      | elem i closedDoors = []
      | otherwise = [(char2num d, l) | (d, l) <- distMap M.! num2char i]

{-
完成！と思ったが、4つめのサンプルで固まる。
マップを見ると、結局重複する状態が大量にあって、
優先度付きキューでの幅優先探索で、同じものを重複して処理させられているようだ。

取得済みの鍵と現在位置が同じものは、距離の最も短いものを調査できたらそれ以降の遅いやつはアウトにする、という
ダイクストラ法の先着順に当たるものがないからこういうことになるんだな。

その情報はいくつになる？
26個の鍵のあり/なしと、27個の現在位置で、27*2^26 = 1,811,939,328
先着順にすれば距離は持っておく必要はないから、1つにつき1ビットでフラグが表現できたとして 226,492,416 バイト
200MBは許容範囲か。
IntSetで持つとして、どうやって整数を割り当てるかだ。Data.Ixは使えない。

鍵がmこあるとき、0≦i＜m番目の鍵がないとき2^i or bit i を立てる
現在位置は 27 * 2^m を足す これで行ける。

ただし、これが正解かはわからない。
別のアプローチとして、経路に制約のある巡回セールスマン問題として解くことができないかと思ったが、
pediaに
> 都市数が20以上になると現実的でない。
> 比較的効率的なアルゴリズム O(n^2 2^n)
とあった。26^2 2^26 は大きい。ぎりぎり行けそうだけども。そもそもTSPに乗せられるかわからん。
-}
