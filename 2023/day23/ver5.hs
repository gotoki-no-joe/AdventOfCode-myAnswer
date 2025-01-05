import Data.Array
import qualified Data.Map as M

import qualified Data.IntMap as IM
import Data.Bits
import Debug.Trace
import System.CPUTime

import Control.Parallel.Strategies

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      wall = replicate w '#'
      fld = listArray ((0, 1),(succ h, w)) $ concat $ wall : ls ++ [wall]
  print $ f h w fld

test12 = runner "sample.txt" part12
main12 = runner "input.txt" part12

-- 予備調査と補助関数

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (i,j) = [(pred i,j), (succ i,j),(i, pred j),(i, succ j)]

checkBranch h w fld = (cond, length ncss)
  where
    ncss =
      [ ncs
      | (p, c) <- assocs fld, c /= '#'
      , let ncs = filter ('#' /=) $ map (fld !) $ neighbors p
      , 2 < length ncs]
    cond = all (all isSlope) ncss

isSlope :: Char -> Bool
isSlope c = elem c "^v<>"

type DistArray = Array Int (IM.IntMap Int)

part12 h w fld = (distG ! 2, maximum dists3)
  where
-- 交差点＝上下左右に矢印が2つ以上ある '.' マス
    crosspoints = (1, 2) : (h, pred w) :
      [ p
      | (p, '.') <- assocs fld
      , 1 < length (filter (isSlope . (fld !)) $ neighbors p)]
    cpN = length crosspoints
-- 交差点の座標から背番号 リスト順なのでスタートは1,ゴールは2
    cpm = M.fromList $ zip crosspoints [1 ..]

-- 交差点を+でマークした地図
-- さらに、出発地点すぐ下と目標地点すぐ上に `v` を置いて交差点に見せかける
    fld1 = fld // (((2,2),'v') : ((pred h, pred w),'v') : [(p,'+') | p <- crosspoints])

-- 指定された交差点の位置から、4方向のうち移動可能な方に出発し、隣の交差点までの距離とその交差点番号を組で返す
    distF p = [dist 1 p q | (c, q) <- zip "^v<>" $ neighbors p, fld1 ! q == c]
    dist d p q
      | fld1 ! q == '+' = (cpm M.! q, d)
      | otherwise = head [dist (succ d) q r | r <- neighbors q, r /= p, fld1 ! r /= '#']
-- いつもの有向グラフ形式
    g = listArray (1, cpN) $ map distF crosspoints

-- グラフは結局一方通行なので、交差点の最も遠い距離は
-- 入ってくる辺の中で最も遠いもの。なのでgの逆を作ってから、それらからの距離の最大値を求める
    revg = accumArray (flip (:)) [] (1, cpN) [(q,(p,d)) | (p,qds) <- assocs g, (q,d) <- qds]
    distG = listArray (1, cpN) $ map distGF $ elems revg
    distGF pds = maximum $ 0 : [d + distG ! p | (p,d) <- pds]

-- パート2

--  無向にしたグラフ
    gg = listArray (1, cpN) $ zipWith (++) (elems g) (elems revg) :: Array Int [(Int,Int)]
-- oarr (スタートから訪問した頂点集合、最長距離）
    oarr0 = listArray (1,cpN) $ IM.singleton (bit 1) 0 : repeat IM.empty
-- darr (ゴールから訪問した頂点集合、最長距離)
    darr0 = listArray (1,cpN) $ IM.empty : IM.singleton (bit 2) 0 : repeat IM.empty

-- (集積した結果, 最前線だけの情報) を1ステップ更新する
    loop :: (DistArray, DistArray) -> (DistArray, DistArray)
    loop (arr, agents) = (arr1, agents1)
      where
        agents1 = accumArray (IM.unionWith max) IM.empty (1, cpN)
            [ (q, IM.singleton vq dpd)
            | (p, vpdp) <- assocs agents                -- pにいるエージェント
            , (vp, dp) <- IM.assocs vpdp                -- 訪問頂点vp, 総距離dp
            , (q, dd) <- gg ! p, not $ testBit vp q     -- pの隣接頂点q,距離dd、qはvpに含まれない
            , let vq = setBit vp q, let dpd = dp + dd ] -- qに進む
        arr1 = accum (IM.unionWith max) arr $ assocs agents1

    (oarrZ, _) = iterate loop (oarr0, oarr0) !! div (pred cpN) 2
    (darrZ, _) = iterate loop (darr0, darr0) !! (pred cpN - div (pred cpN) 2)
--    (oarrZ, darrZ) = runEval $ parTuple2 rpar rseq (oarrZs, darrZs) -- 2並列にできるけど軽いので効果薄い

    peek = zip3 [1 ..] (map IM.size $ elems oarrZ) (map IM.size $ elems darrZ)

    dists2 = zipWith dfun2 (assocs oarrZ) (elems darrZ)

    dfun (v, imo) imd = traceShow (v, IM.size imo, IM.size imd) $ maximum $ 0 :
       [ dp + dq
       | (vp, dp) <- IM.assocs imo  -- 出発地点からここまで来た
       , (vq, dq) <- IM.assocs imd  -- 目標地点からここまで来た
       , popCount (vp .&. vq) == 1] -- 重複は「ここ」一点のみ、という組み合わせ

    dfun2 (v, imo) imd = traceShow (v, IM.size imo, IM.size imd) $ maximum $ 0 :
       [ dp + dq
       | (vp, dp) <- IM.assocs imo
       , (vq, dq) <- associmd
       , popCount (vp .&. vq) == 1]
      where
        associmd = IM.assocs imd

    dists3 = runEval $ parList rpar $
             zipWith dfun2 (assocs oarrZ) (elems darrZ)

main = do
  t0 <- getCPUTime
  main12
  t1 <- getCPUTime
  print $ t1 - t0

-- 並列化成功
-- oarrN, darrN を (cpN + 1 / 2) ステップで作るところを2並列、はやめた。
-- その各要素ごとに、総当たりで経路長を調べるところをcpN並列
