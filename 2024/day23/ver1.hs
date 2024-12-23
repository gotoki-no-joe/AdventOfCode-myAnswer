import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Tuple

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Data.Heap as H

runner i f = do
  pqs <- map parse . lines <$> readFile i
  print $ f pqs

parse l = (as, bs)
  where
    (as,_:bs) = break ('-' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 pqs = length
  [ (p,q,r)
  | (p,qS) <- M.assocs m
  , q <- S.elems qS
  , M.member q m
  , r <- S.elems $ S.intersection qS $ m M.! q
  , elem 't' $ map head [p,q,r]
  ]
  where
    m = fmap S.fromList $ M.fromListWith (++) $ [(min p q,[max p q]) | (p,q) <- pqs]

-- part2

-- データ表現
-- 自分の番号を指しているとき、自分が代表元
type UnionFind s = STUArray s Int Int

-- Union-Find構造体を作る
newUF :: (Int,Int) -> ST s (UnionFind s)
newUF bnds = newListArray bnds $ range bnds

-- 代表元を得る
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
-- 統合が実際に行われたとき、旧代表元2つをペアにして返す（後ろが統合後の代表元）
uniteUF :: UnionFind s -> Int -> Int -> ST s (Maybe (Int, Int))
uniteUF uf i j = do
  a <- getRoot uf i
  b <- getRoot uf j
  if a == b then return Nothing else do
    writeArray uf a b
    return $ Just (a, b)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 pqs = intercalate "," ans
  where
    comps = S.fromList $ map fst pqs ++ map snd pqs
    n = S.size comps
    i2c = listArray (1,n) $ S.elems comps :: Array Int String
    c2i = M.fromDistinctAscList $ map swap $ assocs i2c

    rootI, sizeI :: UArray Int Int
    (rootI, sizeI) = runST $ do
      uf <- newUF (1,n)
      forM_ pqs (\(p,q) -> do
        uniteUF uf (c2i M.! p) (c2i M.! q)
        )
      root <- newArray_ (1,n) :: ST s (STUArray s Int Int)
      size <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
      forM_ [1 .. n] (\i -> do
        r <- getRoot uf i
        writeArray root i r
        readArray size r >>= writeArray size r . succ
        )
      rootI <- freeze root
      sizeI <- freeze size
      return (rootI, sizeI)

    maxSize = maximum $ elems sizeI
    maxRoot = head [i | (i,s) <- assocs sizeI, s == maxSize] -- 一つしかなかった
    ans = [i2c ! i | (i,r) <- assocs rootI, r == maxRoot]

-- 読み間違えた。相互に、だったのか。
-- つまり完全グラフのなしている部分グラフの最大を見つけろと。

{-
専門外で書きぶりの酷い要旨が最初にひっかかる。
https://www.jstage.jst.go.jp/article/ciqs/2006/0/2006_0_JP04/_article/-char/ja/
https://www.jstage.jst.go.jp/article/ciqs/2006/0/2006_0_JP04/_pdf/-char/ja

file:///C:/Users/ohkubo/Downloads/1345_2428~~36~16.pdf
最大クリークというらしい。

辺のあるなしを逆転させたとき、
部分集合で、相互に辺のない部分集合をもってくればいいが、それは全探索と同じだ。

part1のデータ構造を用いて、
今の候補をSとする。
それらの隣接頂点のintersectionの一つを候補とする

一つでなくて、LISみたいに全員一斉にやるべきか。

今の候補を全部一斉に扱う。
一つをSとする。それらの隣接頂点のintersectionで、次に選んでいいものが見つかる。
それを入れて広げたものと、広げてないものを…

並行性がすぎるから、深さ優先探索で順にするべきか。
-}

-- 2やりなおし
-- 2の番号は導入して、計算を高速化しよう。

test3 = runner "sample.txt" part3
main3 = runner "input.txt" part3

part3 pqs = intercalate "," $ map (i2c !) $ IS.elems ans
  where
-- ノード名の背番号
    comps = S.fromList $ map fst pqs ++ map snd pqs
    n = S.size comps
    i2c = listArray (1,n) $ S.elems comps :: Array Int String
    c2i = M.fromDistinctAscList $ map swap $ assocs i2c
-- より大きい隣接ノード番号を持つグラフの配列
    g = amap IS.fromList $ accumArray (flip (:)) [] (1,n) [(min i j,max i j) | (p,q) <- pqs, let i = c2i M.! p, let j = c2i M.! q] :: Array Int IS.IntSet
-- ノード番号nから1まで降りていき、可能な完全部分グラフを作ってみる
-- 番号0まで降りたときに一つ完成して、記録更新したなら更新して戻る
-- 途中、記録がk、今のサイズがs、現在位置がjでk > s + j のとき、全部使っても記録更新できないので打ち切り、で枝刈りする
--    loop maxsize maxlist currentsize currentlist
-- 手続き的でウザいわ。

-- 現在の候補集合 cands ひとつの要素は、構成要素の集合と、それらと接続する要素のintersectionつまり次の候補
-- ∅、完全集合、な初期値singletonから始めて、1～nを順に調べて、
-- 今注目しているkを入れられるもの全てについて、kを追加した版をリストに追加する
-- を全部やればいい。
    initial = H.singleton $ H.Entry 0 (IS.empty, IS.fromList [1 .. n])
    candsN = foldl step initial [1 .. n]
    step cands k = H.union cands $ H.fromList
        [ H.Entry (pred sz) (IS.insert k mem, IS.intersection con $ g ! k)
        | H.Entry sz (mem, con) <- H.toUnsortedList cands, IS.member k con]

    H.Entry _ (ans,_) = H.minimum candsN

-- ヒープを使わず、べったりしたリストで計算して、最後にmaximum使う版と比べてみたい。
-- Nまで終わった瞬間、サイズ最大のが一目でわかるから、いいかと思ったけど、
-- 新規分を H.fromList するには全てをszだけでも計算しないといけないから、
-- 遅延評価によるキャンセルが効かないのかな？H.Entryが!でないなら効くといえば効く。

-- 次に要素を投入できるk'は、IS.intersection con $ g ! k の最小値で簡単にわかるのだから、
-- それを使って、あとでするリストに放りこめば、いちいち舐めなくていい。mutable arrayいるけど。
