import qualified Data.Map as M
import Data.List

{-
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.

という感じ。完全有向グラフだけど、反対向きと同時に影響するので、
実はその和での無向グラフ。なんだこれ。
-}

test = body "test.txt"
main = body "input.txt"

body fn = do
  fi <- readFile fn
  let ls = map parse $ lines fi
  let m = M.fromList ls
  let ans1 = compute1 m
  print ans1
  let ans2 = compute2 m (snd ans1)
  print ans2
  let ans2a = compute2a m (snd ans1)
  print ans2a

parse :: String -> ((String,String),Int)
parse cs = ((as,init bs),f lg $ read ps) where
  [as,_,lg,ps,_,_,_,_,_,_,bs] = words cs
  f "lose" = negate
  f "gain" = id

compute1 :: M.Map (String,String) Int -> (Int,[String])
compute1 m = ans where
  (top:member) = nub $ map fst $ M.keys m
  ans = maximum $ map (\conf -> (score m conf, conf)) $ map (top :) $ permutations member

score :: M.Map (String,String) Int -> [String] -> Int
score m conf = ans where
  pairs = (last conf, head conf) : zip conf (tail conf)
  ans = sum $ map (m M.!) $ pairs ++ map abba pairs

abba (a,b) = (b,a)

{-
*Main> main1
(709,["Mallory","Eric","Carol","Frank","David","George","Alice","Bob"])

8人総当たりは重いな。いや、先頭は固定していいんだけど。

*Main> main1
(709,["Alice","George","David","Frank","Carol","Eric","Mallory","Bob"])

うん、1/8に高速化された。
-}

{-
後半は真面目にやるとさらに重くなるので
まぁやってもできるけどやってはいけなくて、
上で得た並びの中で双方向に最も安いリンクに割って入って
残りを足せばいい。
-}

compute2 :: M.Map (String,String) Int -> [String] -> (Int,(String,String))
compute2 m conf = (sum (map fst scores) - fst worst, snd worst) where
  pairs = (last conf, head conf) : zip conf (tail conf)
  f ab = m M.! ab + m M.! (abba ab)
  scores = zip (map f pairs) pairs
  worst = minimum scores

{-
こたえあわせに、ストレートな方法でもやっておこう。
-}

compute2a :: M.Map (String,String) Int -> [String] -> (Int,[String])
compute2a m members = compute1 $ M.union m $ M.fromList [ pair | m <- members, pair <- [(("you",m),0), ((m,"you"),0)] ]

{-
*Main> main
(709,["Alice","George","David","Frank","Carol","Eric","Mallory","Bob"])
(668,("David","Frank"))
(668,["Alice","George","David","you","Frank","Carol","Eric","Mallory","Bob"])

OKだ。
-}
