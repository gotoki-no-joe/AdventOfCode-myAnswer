import qualified Data.Map as M
import Data.Array

type Rule = M.Map (Char, Char) Char

parse :: String -> (String, Rule)
parse xs = (templ, M.fromList [((a,b),c) | (a:b:' ':'-':'>':' ':c:_) <- rs])
  where
    (templ:_:rs) = lines xs

step :: Rule -> String -> String
step r xs = loop xs
  where
    loop (a:b:xs) = a : r M.! (a,b) : loop (b:xs)
    loop xs = xs

main1 k xs = maximum cnts - minimum cnts
  where
    (t,r) = parse xs
    res = iterate (step r) t !! k
    cnts = filter (0 <) $ elems $ accumArray (+) 0 ('A','Z') [(c,1) | c <- res]

test1 = readFile "sample.txt" >>= print . main1 10

run1 = readFile "input.txt" >>= print . main1 10

-- 後半、ブン回したら大変なことになるよねぇ...
-- 1ステップでn+(n-1)になる。つまりほぼ倍になるので。

test2 = readFile "sample.txt" >>= print . main1 40

run2 = readFile "input.txt" >>= print . main1 40

main2 k xs = unlines $ take k $ iterate (step r) t
  where
    (t,r) = parse xs

test3 = readFile "sample.txt" >>= putStrLn . main2 10
