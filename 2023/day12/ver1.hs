import Data.List.Split
import Data.Array

import Debug.Trace

{-
文字列と、数字列を同時に見ていく。

. がきたとき、ただスルーするしかない。
# がきたとき、#の列が始まるので、矛盾なくはめ込めるか(##～##.まで)判定し、できるならそういう状態にすすむ。不可能なら却下。
? がきたとき、.と思うならスルーする。
              #と思うなら当てはめを試みる。という状態分岐が起きる。
nullに達したとき、数字列が消費できていれば成功。1数える。

または、数字列に合うような、そんな長さの列を全列挙して、矛盾しないものだけ数える、という手も。
適合的にするなら上と変わらんか。
-}

single :: String -> [Int] -> Int
single "" ns = if null ns then 1 else 0 -- 数を消費し尽くしていれば成功
single ('.':cs) ns = single cs ns -- '.' はスルー
single ('#':cs) (n:ns) = if check (pred n) cs then single (drop n cs) ns else 0
single ('?':cs) ns = single ('.':cs) ns + single ('#':cs) ns
single cs [] = if notElem '#' cs then 1 else 0 -- #を消費し尽くしていれば成功

check k [] = k == 0
check 0 (c:cs) = c /= '#' -- '.'か'?'ならヨシ
check k (c:cs) = c /= '.' && check (pred k) cs -- '#'か'?'ならヨシ

part1 fn = do
  ls <- map parse . lines <$> readFile fn
  print $ sum $ map (uncurry single) ls

parse :: String -> (String, [Int])
parse l = (cs, ns)
  where
    [cs,bs] = words l
    ns = map read . wordsBy (',' ==) $ bs

{-
ghci> part1 "sample.txt"
21
ghci> part1 "input.txt"
7670
-}

{-
5倍にするのに伴って、DP化する。
真面目にするのでもいいけど、適当にキャッシュするだけでも何とかならんかなぁとも思う。
ただ、文字列と数字列を引数として直に扱うのもアレなので、
注目点が先頭からいくつ目か、だけにして、実際の値は配列から読むことにする。

何か変だと思ったら、文字列の間に?を追加する必要があるのか！数字列の方はそのままでいいが。
なので、長さcLなcsは、cs ? cs ? cs ? cs ? cs となって、長さはcL*5+4になるし、
アクセサはもうちょっと面倒なのか。
-}

single2 :: String -> [Int] -> Int
single2 cs ns = cnt ! (0,0)
  where
    cL = length cs
    cL5 = 5 * cL + 4
    nL = length ns
    nL5 = 5 * nL
    cA = listArray (0, cL) (cs ++ "?")
    nA = listArray (0, pred nL) ns
    c i = cA ! mod i (succ cL)
    cnt = listArray ((0,0), (cL5, nL5))
          [f i j | i <- [0 .. cL5], j <- [0 .. nL5]]
    f i j
      | i == cL5 = if j == nL5 then 1 else 0 -- 文字を使い切ってたら数も使い切ってないとダメ
--      | j == nL5 = if all (('#' /=) . c) [i .. pred cL5] then 1 else 0 -- 数を使い切ってたら #,? が残っていたらダメ
      | ci == '.' = cnt ! (succ i, j)
      | ci == '#' =                     next
      | ci == '?' = cnt ! (succ i, j) + next
      | otherwise = trace (show (i, ci, j, cL5, nL5, cond1)) $ error "hoge"
      where
        ci = c i
        nj = nA ! mod j nL
        -- i から + nj-1 に '##-##' が入るか
        cond1 = j < nL5 && i + pred nj < cL5 && all (('.' /=) . c . (i +)) [1 .. pred nj]
        -- その続きがまだあって . が入るか
        cond2 = i + nj < cL5 && c (i + nj) /= '#'
        -- これでちょうどcsが尽きるか
        cond3 = i + nj == cL5
        -- それを使う続き
        next
          | not cond1 = 0
          | cond2     = cnt ! (i + succ nj, succ j)
          | cond3     = cnt ! (i + nj, succ j)
          | otherwise = 0

part2 fn = do
  ls <- map parse . lines <$> readFile fn
  print $ sum $ map (uncurry single2) ls

single3 cs ns = single (cs ++ '?' : cs ++ '?' : cs ++ '?' : cs ++ '?' : cs) (ns ++ ns ++ ns ++ ns ++ ns)
-- さすがに無理。

{-
ghci> part2 "sample.txt"
525152
ghci> part2 "input.txt" 
157383940585037
-}

{-
最後が # で終わっているときの扱いを、普通ならpart1で全てバグ出しできるところを、
check k [] = k == 0 カウント終わって . を探す代わりに末端ならヨシ
(drop n cs) csからn文字引いたら取り過ぎだけど、dropはエラーにしないからヨシ
で気づかず通り過ぎてしまい、part2になってから踏んだせいで時間をくったわ。

連続する.は全て1つに圧縮してしまって構わなかったな、そういえば。
-}
