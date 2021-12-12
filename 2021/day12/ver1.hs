{-
深さ優先探索で、制約を満たす経路を数え上げる他はなさそうだ。
-}

import qualified Data.Map as M
import Data.List
import Data.Char

type Graph = M.Map String [String]

parse :: String -> Graph
parse xs = M.fromListWith union $
    [ p
    | l <- lines xs
    , let (a,'-':b) = span ('-' /=) l
--    , p <- [(a,[b]),(b,[a])]
    , p <- [(a,[b]) | a /= "end", b /= "start"] ++ [(b,[a]) | b /= "end", a /= "start"]
    ]

-- @gotoki_no_joe
dfs :: (s -> ([x],[s])) -> s -> [x]
dfs f i = loop [i]
  where
    loop [] = []
    loop (x:xs) = let (a,b) = f x in a ++ loop (b ++ xs)

{-
グラフは、常に使える情報
現在位置と、訪問済み頂点集合が、現在の状態。
現在位置でグラフをひいて、隣接する頂点でまだ訪問していないもの全てについて、次状態を作る。
それが "end" のものを終了状態、それ以外を次状態に振り分けておわり。

単なる経路でなくて、先頭が小文字のもののみvisitedに入れる、を忘れていた。
-}

type State = (String,[String])

step :: M.Map String [String] -> State -> ([State],[State])
step g (current,visited) =
  ( [("end", union ["end"] visited) | elem "end" gc]
  , [(adj, v1 adj) | adj <- gc \\ visited, adj /= "end"]
  )
  where
    gc = g M.! current
    v1 adj = if isLower (head adj) then adj : visited else visited

initial = ("start",[])
main1 fn = readFile fn >>= print . length . flip dfs initial . step . parse

test11 = main1 "sample1.txt"
test12 = main1 "sample2.txt"
test13 = main1 "sample3.txt"
run1 = main1 "input.txt"

{-
*Main> test11
10
*Main> test12
19
*Main> test13
226
*Main> run1
4186
-}

{-
後半やらしいなwww
そして、startに戻る辺は最初からグラフに入れなければよいことに気づいた。
endから出ていく線もいらんけど、それは止めるから問題ない。
-}

type State2 = (String,Bool,[String])

initial2 = ("start",True,[])

{-
availでないとき、phase1同様に、gc \\ visited のみ考える
availなとき、intersect gc visited は、availをFalseにしつつ続行できるものとして作る
-}

step2 :: M.Map String [String] -> State2 -> ([State2],[State2])
step2 g (current,avail,visited) =
  ( [("end", avail, union ["end"] visited) | elem "end" gc]
  , [(adj, avail, v1 adj) | adj <- gc \\ visited, adj /= "end"] ++
    [(adj, False, visited) | avail, adj <- intersect gc visited]
  )
  where
    gc = g M.! current
    v1 adj = if isLower (head adj) then adj : visited else visited

main2 fn = readFile fn >>= print . length . flip dfs initial2 . step2 . parse

test21 = main2 "sample1.txt"
test22 = main2 "sample2.txt"
test23 = main2 "sample3.txt"
run2 = main2 "input.txt"

{-
*Main> test21
36
*Main> test22
103
*Main> test23
3509
*Main> run2
92111
一瞬考えこむ時間がかかった。
-}
