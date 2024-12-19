import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Debug.Trace

runner i f = do
  l:_:ds <- lines <$> readFile i
  let ts = words $ filter (',' /=) l
  print $ f ts ds

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [String] -> [String] -> Int
part1 ts ds = length $ filter (avail ts) ds

avail0 ts pat = loop pat
  where
    tls = [(t, length t) | t <- ts]
    loop "" = True
    loop xs = or [loop $ drop l xs | (t,l) <- tls, isPrefixOf t xs]

{-
ヤバい。こんな実装だとTestすら通らない。
調査済みの列と、調査予定の列をSetで重複なしに管理するか。
長さの順に優先度付きキューで。しかしそれはSetで代用できる。

うん、getLineとかやってボケてました。ははっ。
だけど、sampleなら上でも通ったけど、本番はキツかった。
なので下書いてよかった。
-}

avail ts pat = loop S.empty (S.singleton (length pat, pat))
  where
    tls = [(t, length t) | t <- ts]
    loop :: S.Set (Int,String) -> S.Set (Int,String) -> Bool
    loop done new
      | S.null new = False
      | len == 0   = True
      | S.member ent done = loop done new1
      | otherwise = loop (S.insert ent done) (S.union new1 new2)
      where
        (ent@(len, rest), new1) = S.deleteFindMin new
        new2 = S.fromList
            [ (len - l, b)
            | (t,l) <- tls
            , let (a,b) = splitAt l rest, t == a
            , let ent1 = (len - l, b)
            , notElem ent1 done
            , notElem ent1 new]

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
      where
        f mf "" = ([], 1)
        f mf xs = (bs, sum $ map mf bs)
          where
            bs = [b | (t,l) <- tls, let (a,b) = splitAt l xs, a == t]
