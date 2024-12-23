import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.IntSet as IS

import Data.Array.IArray

import Data.List

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

part2 pqs = (post ans1, post ans2)
  where
-- 整数に写す
    pS = S.fromList $ "" : concat pqs
    n = pred $ S.size pS
    g :: Array Int IS.IntSet
    g = amap (IS.fromList . (0 :)) $ accumArray (flip (:)) [] (1, n) -- 番兵入り
        [maxmin ij | pq <- pqs, let ij = map (flip S.findIndex pS) pq]

    maxmin (a:b:_)
      | a >= b    = (a,b)
      | otherwise = (b,a)

    post = intercalate "," . map (flip S.elemAt pS) . IS.elems

-- 幅優先探索ではメモリヘビーなので、深さ優先探索に切り替える。
-- スタックを自前で管理して、全ての候補をリストで吐き出す。
    ans4 = snd $ maximum cands4
      where
-- 次に調べるk, (サイズ, 選択されたノード, 全結合されているノード) のスタックを引数にとり、
-- k=0になったら吐き出して戻るdfs
        cands4 = dfs [(n, (0, IS.empty, IS.fromDistinctAscList [0 .. n]))]
        dfs :: [(Int, (Int, IS.IntSet, IS.IntSet))] -> [(Int, IS.IntSet)]
        dfs [] = []
        dfs ((0, (sz, mem, _)):stk) = (sz, mem) : dfs stk -- ひとつ完成
        dfs ((k, ent@(sz, mem, con)):stk) =
            dfs $ (IS.findMax con1, ent1) : (fromJust $ IS.lookupLT k con, ent) : stk
          where
            con1 = IS.intersection con $ g ! k
            ent1 = (succ sz, IS.insert k mem, con1)
-- すっごいシンプルにできた。
-- 候補だらけで死ぬと思ったけど、普通に答え出たな。


-- ひとつ完成するたびに出力する代わりに、
-- dfsが「現在のベストスコア」と「その答え」を追跡して、
-- 完成時にはそれを更新するだけ。
-- また、見込みがなくなった候補はどんどん捨てる、←入ってなくない？
-- 候補が尽きたら結果をひとつだけ吐き出す、
-- という枝刈り拡張版
    ans5 = dfs 0 IS.empty [(n, (0, IS.empty, IS.fromDistinctAscList [0 .. n]))]
      where
        dfs :: Int -> IS.IntSet -> [(Int, (Int, IS.IntSet, IS.IntSet))] -> IS.IntSet
        dfs _ ans [] = ans
        dfs candsz cand ((k, ent@(sz, mem, con)):stk)
          | k == 0       = dfs candsz cand stk  -- この時点でk,memがベストならそれは既にcandsz,candに入っているはず
          | candsz > k + sz = dfs candsz cand stk -- 全部入れても届かないので打ち切り
          | candsz == sz = dfs sz1    mem1 stk1 -- sz1に広がるので記録更新
          | otherwise    = dfs candsz cand stk1
          where
            sz1  = succ sz
            con1 = IS.intersection con $ g ! k
            mem1 = IS.insert k mem
            ent1 = (sz1, mem1, con1)
            stk1 = (IS.findMax con1, ent1) : (fromJust $ IS.lookupLT k con, ent) : stk

{-
スタックにpushしたばかりのものを次にすぐpopするのを、引数を増やすことで最適化できんかと思ったが、
完全な空リストになった瞬間へ、空リストのときする計算を混ぜ込むのがちょっとひっかかる。
でもこれについては可能。

    ans6 = dfs 0 IS.empty (n1, (0, IS.empty, IS.fromDistinctAscList [-1 .. n1])) []
      where
        dfs :: Int -> IS.IntSet -> (Int, (Int, IS.IntSet, IS.IntSet)) -> [(Int, (Int, IS.IntSet, IS.IntSet))] -> IS.IntSet
        dfs candsz cand (k, ent@(sz, mem, con)) stk
          | k == -1, null stk = cand
          | k == -1      = dfs candsz cand (head stk) (tail stk)  -- この時点でk,memがベストならそれは既にcandsz,candに入っているはず
          | candsz == sz = dfs sz1    mem1 (IS.findMax con1, ent1) stk1 -- sz1に広がるので記録更新
          | otherwise    = dfs candsz cand (IS.findMax con1, ent1) stk1
          where
            sz1  = succ sz
            con1 = IS.intersection con $ g ! k
            mem1 = IS.insert k mem
            ent1 = (sz1, mem1, con1)
            stk1 = (fromJust $ IS.lookupLT k con, ent) : stk

-- 動いたけど、この変換はコンパイラの仕事だ。
-}

-- それよりむしろ、最後に答えを一つだけ吐いて終わる形式なら、
-- 呼び出しスタックに再帰を押し込むことができる。
-- 普通のコードっぽくなる。

    ans7 = snd $ dfs n (0, IS.empty) (IS.fromDistinctAscList [0 .. n]) (0, IS.empty)
      where
        dfs :: Int -> (Int, IS.IntSet) -> IS.IntSet -> (Int, IS.IntSet) -> (Int, IS.IntSet)
        dfs 0 _ _ cand = cand
        dfs k ent@(sz, mem) con cand@(csz, _)
          | csz > k + sz = cand -- ここからでは巻き返せない
          | otherwise = dfs (fromJust $ IS.lookupLT k con) ent con $
                        dfs (IS.findMax con1) (sz1, mem1) con1 $
                        if csz == sz then (sz1, mem1) else cand -- kを採用すると記録更新
          where
            sz1  = succ sz
            con1 = IS.intersection con $ g ! k
            mem1 = IS.insert k mem

-- 逆にans7をデグレしたもの
    ans1 = snd $ dfs n (0, IS.empty) (IS.fromDistinctAscList [0 .. n])
      where
        dfs :: Int -- 今から調べる頂点番号
            -> (Int, IS.IntSet) -- 今調査している状態、サイズと頂点集合
            -> IS.IntSet        -- と、次に選べる、全結合になっている頂点集合
            -> (Int, IS.IntSet) -- 答え、サイズと頂点集合
        dfs 0 ent _ = ent                       -- 底まで降りたら答えを返す
        dfs k ent@(sz, mem) con = max res1 res2 -- 分岐して戻ってきた値の大きい方
          where
            sz1  = succ sz
            mem1 = IS.insert k mem
            con1 = IS.intersection con $ g ! k
            res1 = dfs (IS.findMax con1) (sz1, mem1) con1     -- kを入れて再帰
            res2 = dfs (fromJust $ IS.lookupLT k con) ent con -- kを入れずに再帰

-- 上のans1にans7の枝刈りを入れ直した最終版
    ans2 = snd $ dfs n (0, IS.empty) (IS.fromDistinctAscList [0 .. n]) (0, IS.empty)
      where
        dfs :: Int -- 今から調べる頂点番号
            -> (Int, IS.IntSet) -- 今調査している状態、サイズと頂点集合
            -> IS.IntSet        -- と、次に選べる、全結合になっている頂点集合
            -> (Int, IS.IntSet) -- 現時点での最適解
            -> (Int, IS.IntSet) -- 答え、サイズと頂点集合
        dfs 0 _ent _ best = best               -- bestは常時更新するので、_entがベスト解ならbestも同じ内容
        dfs k ent@(sz, mem) con best@(bestsz,_)
          | k + sz < bestsz = best                       -- 挽回不能な差がついている
          | otherwise =
            dfs (fromJust $ IS.lookupLT k con) ent con $ -- kを入れずに再帰
            dfs (IS.findMax con1) ent1 con1 $            -- kを入れて再帰
            if sz == bestsz then ent1 else best          -- sz==bestszならsz1>bestszなので更新する
          where
            sz1  = succ sz
            mem1 = IS.insert k mem
            con1 = IS.intersection con $ g ! k
            ent1 = (sz1, mem1)
