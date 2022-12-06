-- 2022-11-14

import qualified Data.Map as M
import Data.List

main1 = do
  co <- readFile "input.txt"
  print $ part1 $ lines co

part1 ls = (minimum dps, maximum dps)
  where
    ccds = map parse ls
    distMap = M.fromList [(minMax c1 c2, d) | (c1,c2,d) <- ccds]
    cities = nub [c | (c1,c2,_) <- ccds, c <- [c1,c2]]
    dps = [(fullDist cs, cs) | cs <- permutations cities]

    distOf c1 c2 = distMap M.! minMax c1 c2
    fullDist cs = sum $ zipWith distOf cs (tail cs)

parse :: String -> (String,String,Int)
parse xs = (ws !! 0, ws !! 2, read $ ws !! 4)
  where
    ws = words xs

minMax :: Ord a => a -> a -> (a, a)
minMax a b = if a <= b then (a,b) else (b,a)
