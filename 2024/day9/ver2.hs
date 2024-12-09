import Data.Char
import Data.List

--import qualified Data.IntSet as IS
import Data.Array

import Debug.Trace

sample = "2333133121414131402"

runner f = readFile "input.txt" >>= print . f . head . lines

test2 = part2 sample
main2 = runner part2

part2 :: String -> Int
part2 l = sum scores
  where
--    len = length l
--    fidmax = div (succ len) 2
    lens = map digitToInt l -- 各ブロックの長さ
    poss = scanl (+) 0 lens -- 各ブロックの開始位置
    spacelps0 = fmap sort $ accumArray (flip (:)) [maxBound] (0,9) $ -- 空白ブロックを長さで分類して、開始位置の昇順のリスト
              map snd $ filter fst $ zip (cycle [False,True]) $ zip lens poss
    fileLPs = map snd $ filter fst $ zip (cycle [True,False]) $ zip lens poss -- 各ファイルの長さと初期位置のリスト
    (_, scores) = mapAccumR step spacelps0 $ zip [0 ..] fileLPs
-- 空白の状況spacelpsで、長さflenのものを移動させられるならして、空白を更新して、位置からスコアを出力する
    step spacelps (fid, (flen, fpos))
      | null spcand = (spacelps, score fpos)
      | otherwise   = (spacelps1, score p)
      where
-- flen以上の空白ブロックの位置で最左のものと、その長さの対、つまり候補
        spcand = [(p,l) | l <- [flen .. 9], let p = head $ spacelps ! l, p < fpos]
-- fidの落ち着く先がpのとき、スコアは fid * sum[p .. p+flen-1] = fid * (2p + flen - 1) * flen / 2
        score p = fid * div ((p + p + pred flen) * flen) 2
-- fidの落ち着く先決定
        (p, l) = minimum spcand
-- 空白から長さlの先頭を削除し、l - flen に p + flen スタートのものを挿入する
        spacelps1 = spacelps // [(l, tail $ spacelps ! l), (l - flen, insert (p + flen) $ spacelps ! (l - flen))]

-- リストに番兵を入れておいて head が落ちないようにするか、
-- take 1 でリストとして取り出して concatMap で答えを集めるか。

{-
immutable arrayを (//) していても
ghci> test2
2858
ghci> main2
6361380647183
一瞬で答えが出た。想定解だなこれ。むふふ。
-}
