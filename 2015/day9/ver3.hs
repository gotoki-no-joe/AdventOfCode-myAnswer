-- 2025/1/11
import qualified Data.Map as M

import Data.List


runner i f = readFile i >>= print . f . map parse . lines

parse :: String -> (String,String,Int)
parse xs = (ws !! 0, ws !! 2, read $ ws !! 4)
  where
    ws = words xs

part12 ccds = minmaximum $ map fullDist $ permutations cities
  where
    distMap :: M.Map (String,String) Int
    distMap = M.fromList $ concat [[((c1, c2), d), ((c2, c1), d)] | (c1,c2,d) <- ccds]

    fullDist :: [String] -> Int
    fullDist cs = sum $ map (distMap M.!) $ zip cs (tail cs)

    cities = nub $ concat [[c1, c2] | (c1,c2,_) <- ccds]

test12 = runner "test.txt" part12
main12 = runner "input.txt" part12

minmaximum :: Ord a => [a] -> (a,a)
minmaximum (x:xs) = foldl' step (x,x) xs
  where
    step lbub@(lb,ub) x
      | x < lb = (x, ub)
      | ub < x = (lb, x)
      | otherwise = lbub
