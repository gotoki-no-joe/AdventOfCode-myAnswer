import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Array

runner i f = do
  l:_:ds <- lines <$> readFile i
  let ts = words $ filter (',' /=) l
  print $ f ts ds

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [String] -> [String] -> Int
part1 ts ds = length $ filter avail ds
  where
    tls = [(t, length t) | t <- ts]
    avail pat = memoize f pat
    f mf "" = ([], True)
    f mf xs = (bs, any mf bs)
      where
        bs = [b | (t,l) <- tls, let (a,b) = splitAt l xs, a == t]

{-
ていうか、part2解いてから思うと、カウントの代わりに Bool / Or でよいのではないか。
…できちゃった。
-}

{-
そしてPart2は、場合の数を数えるってことなので、上のSetを場合の数カウントのMapに代える感じなんだけど、
これもしかしてmemoizeの出番か？
patからtで削って作れる全てのtailsについて、その個数を数えればいいのだから。そうだね。
-}

memoize :: Ord t => ((t -> s) -> t -> ([t], s)) -> t -> s
memoize fya x = m M.! x
  where
    m = loop M.empty (S.singleton x)
    loop old new
      | S.null new = old
      | otherwise  = loop old1 new1
      where
        (kvs, jss) = unzip [((k,v),js) | k <- S.elems new, let (js, v) = fya (m M.!) k]
        old1 = M.union old $ M.fromList kvs
        new1 = S.fromList $ concatMap (filter (flip M.notMember old1)) jss

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [String] -> [String] -> Int
part2 ts ds = sum $ map countWays ds
  where
    tls = [(t, length t) | t <- ts]
    countWays pat = memoize f pat
    f mf "" = ([], 1)
    f mf xs = (bs, sum $ map mf bs)
      where
        bs = [b | (t,l) <- tls, let (a,b) = splitAt l xs, a == t]

--- 配列によるDPな別解。

-- 添え字範囲bndsの位置iのマスに対して、関数nfは
-- 答えを求めるのに必要な再帰呼び出しの添え字のリスト js と
-- j と DP 結果のタプルのリスト jds から i jds で結果を求める関数
-- を対で返す
-- 結果は、添え字範囲の計算結果配列
arrayDP :: Ix i => (i, i) -> (i -> ([i], [(i, b)] -> b)) -> Array i b
arrayDP bnds nf = dpArr
  where
    dpArr = listArray bnds [fi [(j, dpArr ! j) | j <- js] | i <- range bnds, let (js, fi) = nf i]

part2a :: [String] -> [String] -> Int
part2a ts ds = sum $ map countWays ds
  where
    tls = [(t, length t) | t <- ts]
    countWays pat = arrayDP (0, len) nf ! 0
      where
        len = length pat
-- デザインの先頭 k 文字を落としたものを作る方法の場合の数
        nf k
          | k == len = ([], const 1)
          | otherwise = (ls, sum . map snd)
          where
            ls = [k + l | (t,l) <- tls, isPrefixOf t $ drop k pat]

{-
ghci> main2
705756472327497
ghci> runner "input.txt" part2a
705756472327497
-}

part1a :: [String] -> [String] -> Int
part1a ts ds = length $ filter avail ds
  where
    tls = [(t, length t) | t <- ts]
    avail pat = arrayDP (0, len) nf ! 0
      where
        len = length pat
-- デザインの先頭 k 文字を落としたものが作れる
        nf k
          | k == len = ([], const True)
          | otherwise = (ls, or . map snd)
          where
            ls = [k + l | (t,l) <- tls, isPrefixOf t $ drop k pat]

{-
ghci> main1
304
ghci> runner "input.txt" part1a
304
-}
