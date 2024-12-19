import Data.Array

import qualified Data.Set as S

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Array (Int,Int) Char -> Int
part1 arr = sum [S.size s | ('0', s) <- zip (elems arr) (elems score)] -- 標高0のそれの個数の和
  where
    score = dp (bounds arr) gather (upways arr)   -- それぞれのマスから行ける9のマスの位置集合
    gather i jds                                  -- 集める計算
      | arr ! i == '9' = S.singleton i            -- 頂上は自分自身
      | otherwise      = S.unions $ map snd jds   -- 周囲の結果を統合

upways arr p@(i,j) =                                          -- 集める元のマスリスト
  [ q
  | q <- [(pred i, j), (succ i, j), (i, pred j), (i, succ j)] -- 上下左右に隣接して
  , inRange (bounds arr) q                                    -- はみ出していなくて
  , succ (arr ! p) == arr ! q]                                -- 標高が1大きい

-- 範囲bndsの位置iのマスに対して、n i で与えられる周囲のマス j について
-- jとDP結果のタプルのリストjdsから f i jds でDP結果を求める
dp :: Ix i => (i, i) -> (i -> [(i, b)] -> b) -> (i -> [i]) -> Array i b
dp bnds f n = dpArr
  where
    dpArr = listArray bnds [ f i [(j, dpArr ! j) | j <- n i] | i <- range bnds]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Array (Int,Int) Char -> Int
part2 arr = sum [s | ('0', s) <- zip (elems arr) (elems score)] -- 標高0の経路数の和
  where
    score = dp (bounds arr) gather (upways arr) -- それぞれのマスから行ける9のマスへの経路数
    gather i jds                                -- 集める計算
      | arr ! i == '9' = 1                      -- 頂上は経路数1
      | otherwise      = sum $ map snd jds      -- 周囲の経路数の合計
