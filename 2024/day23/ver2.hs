import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Array.IArray

import qualified Data.Heap as H
import Data.List

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe

import Debug.Trace

runner i f = do
  pqs <- map parse . lines <$> readFile i
  print $ f pqs

parse l = [as, bs]
  where
    (as,_:bs) = break ('-' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 pqs = length
  [ (p,q,r)
  | (p,qS) <- M.assocs m
  , q <- S.elems qS
  , M.member q m -- q, r は m にキーとして現れない可能性がある
  , r <- S.elems $ S.intersection qS $ m M.! q
  , elem 't' $ map head [p,q,r]
  ]
  where
    m = fmap S.fromList $ M.fromListWith (++) $ [(minimum pq,[maximum pq]) | pq <- pqs]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 pqs = (post ans4, post ans5)
  where
-- 整数に映す
    pS = S.fromList $ concat pqs
    ub = pred $ S.size pS
    g :: Array Int IS.IntSet
    g = amap IS.fromList $ accumArray (flip (:)) [] (0, ub)
        [(minimum ij, maximum ij) | pq <- pqs, let ij = map (flip S.findIndex pS) pq]
-- ヒープを使う
    initial = H.singleton $ H.Entry 0 (IS.empty, IS.fromDistinctAscList [0 .. ub])
    H.Entry _ (ans, _) = H.minimum $ foldl step initial [0 .. ub]
    step cands k = H.union cands $ H.fromList
        [ H.Entry (pred sz) (IS.insert k mem, IS.intersection con $ g ! k)
        | H.Entry sz (mem, con) <- H.toUnsortedList cands, IS.member k con]
    post = intercalate "," . map (flip S.elemAt pS) . IS.elems
-- ヒープのバケツを使う
    n = S.size pS
    g1 :: Array Int IS.IntSet
    g1 = amap (IS.insert n) g -- 番兵を潜ませる
    ans1 = runST $ do
      bags <- newArray (0, n) H.empty :: ST s (STArray s Int (H.Heap (H.Entry Int (IS.IntSet, IS.IntSet))))
      insert2B bags 0 (H.Entry 0 (IS.empty, IS.fromDistinctAscList [0 .. n]))
      forM_ [0 .. ub] (\k -> do
        cands <- readArray bags k
        writeArray bags k H.empty -- メモリに配慮
        forM_ (H.toUnsortedList cands) (\ent@(H.Entry sz (mem, con)) -> do
          let con1 = IS.intersection con $ g1 ! k
          let ent1 = H.Entry (pred sz) (IS.insert k mem, con1)
          insert2B bags (IS.findMin con1) ent1
          insert2B bags (fromJust $ IS.lookupGT k con) ent
          )
        )
      H.Entry _ (ans, _) <- H.minimum <$> readArray bags n
      return ans

-- 袋のi番目のヒープにentを追加する
    insert2B bags i ent = do
      cands <- readArray bags i
      writeArray bags i $ H.insert ent cands

-- リストのバケツを使う
-- 全部終わってから maximumBy で IS.size しながら最大を探すと、IS.sizeがO(N)なので台無しになる。szフィールドは必要
    ans2 = runST $ do
      bags <- newArray (0, n) [] :: ST s (STArray s Int [(Int, IS.IntSet, IS.IntSet)])
      insert2L bags 0 (0, IS.empty, IS.fromDistinctAscList [0 .. n])
      forM_ [0 .. ub] (\k -> do
        cands <- readArray bags k
        writeArray bags k [] -- メモリに配慮
        forM_ cands (\ent@(sz, mem, con) -> do
          let con1 = IS.intersection con $ g1 ! k
          let ent1 = (succ sz, IS.insert k mem, con1)
          insert2L bags (IS.findMin con1) ent1
          insert2L bags (fromJust $ IS.lookupGT k con) ent
          )
        )
      (_,ans2,_) <- maximum <$> readArray bags n
      return ans2

-- 袋のi番目のヒープにentを追加する
    insert2L bags i ent = do
      cands <- readArray bags i
      writeArray bags i (ent : cands)

-- リストのバケツを使い、現在のベストスコアを追跡することで、見込みのない候補をスターリンする
-- バケツのNに入れるときは、ベストスコアのものだけ入れる
    ans3 = runST $ do
      bags <- newArray (0, n) [] :: ST s (STArray s Int [(Int, IS.IntSet, IS.IntSet)])
      writeArray bags 0 [(0, IS.empty, IS.fromDistinctAscList [0 .. n])]
      bestsizeN <- foldM (\bs k -> do
        cands <- readArray bags k
        writeArray bags k [] -- メモリに配慮
        let szmax = maximum $ map getsz cands -- 今回処理する候補のサイズの最大値
        let bs1 = max bs $ succ szmax         -- 更新されるベストスコア
        let cands1 = filter (\e -> bs1 <= n - k + getsz e) cands -- 意味のある候補だけ残す
-- kを入れるとき、残り要素はN-k
-- これを全て取り込めたときのサイズ sz + n - k < bs なら、この要素については放棄する
        forM_ cands1 (\ent@(sz, mem, con) -> do
          let con1 = IS.intersection con $ g1 ! k
          let ent1 = (succ sz, IS.insert k mem, con1)
          insert2La bs1 bags (IS.findMin con1) ent1
          insert2La bs1 bags (fromJust $ IS.lookupGT k con) ent
          )
        return bs1
        ) 0 [0 .. ub]
      (_,ans3,_) <- head . filter ((bestsizeN ==) . getsz) <$> readArray bags n
      return ans3

    getsz (sz,_,_) = sz

-- 袋のi番目のヒープにentを追加する
-- i==Nのとき、サイズがbs未満なら無視する
    insert2La bs bags i ent
      | i == n, getsz ent < bs = return ()
      | otherwise = do
          cands <- readArray bags i
          writeArray bags i (ent : cands)

-- 幅優先探索ではメモリヘビーなので、深さ優先探索に切り替える。
-- スタックを自前で管理して、全ての候補をリストで吐き出す。
    ans4 = snd $ maximum cands4
      where
-- 次に調べるk, (サイズ, 選択されたノード, 全結合されているノード) のスタックを引数にとり、
-- k=Nになったら吐き出して戻るdfs
        cands4 = dfs [(0,(0,IS.empty, IS.fromDistinctAscList [0 .. n]))]
        dfs :: [(Int, (Int, IS.IntSet, IS.IntSet))] -> [(Int, IS.IntSet)]
        dfs [] = []
        dfs ((k, ent@(sz, mem, con)):stk)
          | k == n = (sz, mem) : dfs stk -- ひとつ完成
          | otherwise = dfs $ (IS.findMin con1, ent1) : (fromJust $ IS.lookupGT k con, ent) : stk
          where
            con1 = IS.intersection con $ g1 ! k
            ent1 = (succ sz, IS.insert k mem, con1)
-- すっごいシンプルにできた。
-- 候補だらけで死ぬと思ったけど、普通に答え出たな。

-- ひとつ完成するたびに出力する代わりに、
-- dfsが「現在のベストスコア」と「その答え」を追跡して、
-- 完成時にはそれを更新するだけ。
-- また、見込みがなくなった候補はどんどん捨てる、
-- 候補が尽きたら結果をひとつだけ吐き出す、
-- という枝刈り拡張版
    ans5 = dfs 0 IS.empty [(0,(0,IS.empty, IS.fromDistinctAscList [0 .. n]))]
      where
        dfs :: Int -> IS.IntSet -> [(Int, (Int, IS.IntSet, IS.IntSet))] -> IS.IntSet
        dfs _ ans [] = ans
        dfs candsz cand ((k, ent@(sz, mem, con)):stk)
          | k == n, candsz < sz = dfs sz mem stk -- ひとつ完成
          | k == n              = dfs candsz cand stk
          | candsz == sz = dfs sz1 mem1 stk1
          | otherwise    = dfs candsz cand stk1
          where
            sz1  = succ sz
            con1 = IS.intersection con $ g1 ! k
            mem1 = IS.insert k mem
            ent1 = (sz1, mem1, con1)
            stk1 = (IS.findMin con1, ent1) : (fromJust $ IS.lookupGT k con, ent) : stk
