import Data.Graph
import Data.List
import qualified Data.Map as M

{-
input.txtのパースが一苦労だ。
色名をキーにしたmapを作るべきか。
-}

sample ="light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

gen dat = graphFromEdges $ map procline $ lines dat

procline li
  | head ws == "no" = ((),key, [])
  | True = ((), key, unfoldr f ws)
  where
    (w1:w2:"bags":"contain":ws) = words li
    key = w1 ++ ' ' : w2
    f (wd:w1:w2:bag:ws) = Just (w1 ++ ' ' : w2, ws)
    f [] = Nothing

phase1 dat = pred $ length $ reachable (transposeG g) shinygold
  where
    (g, v2tf, k2mvf) = gen dat
    Just shinygold = k2mvf "shiny gold"

{-
そして、shiny goldに到達できる色を全て挙げろと言っている。
Graphモジュールを使って計算しよう。
-}

test1 = phase1 sample

ans1 = do
  co <- readFile "input.txt"
  print $ phase1 co

{-
パート２
循環があると答えは無限になるから、そういうことはないと仮定して話を進める。
とすると、遅延mapで、自分以下のnode数を一発で求めることができる。
-}

phase2 dat = pred $ m M.! "shiny gold"
  where
    kcns = map procline2 $ lines dat
    m = M.fromList [ (k, succ $ sum [ n * m M.! c | (c,n) <- cns ]) | (k,cns) <- kcns ]

procline2 li
  | head ws == "no" = (key, [])
  | True = (key, unfoldr f ws)
  where
    (w1:w2:"bags":"contain":ws) = words li
    key = w1 ++ ' ' : w2
    f (wd:w1:w2:bag:ws) = Just ((w1 ++ ' ' : w2, read wd), ws)
    f [] = Nothing

test2 = phase2 sample

sample2 = "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."

test3 = phase2 sample2

ans2 = do
  co <- readFile "input.txt"
  print $ phase2 co

{-
*Main> test1
4
*Main> ans1
211
*Main> test2
32
*Main> test3
126
*Main> ans2
12414
-}
