-- 2022-11-17

import Data.List
import qualified Data.Map as M

main1 = do
  co <- readFile "input.txt"
  print $ part1 $ lines co

part1 ls = maximum $ map score $ permutations ps
  where
    pqxs = map parse ls
    (p1:ps) = nub $ map (fst . fst) pqxs
    pqxm = M.fromList pqxs
    score ps = sum
      [pqxm M.! (p,q) + pqxm M.! (q,p) | (p,q) <- zip (p1 : ps) (ps ++ [p1])]

parse :: String -> ((String,String), Int)
parse xs = ((w0, init w10), (if w2 == "gain" then id else negate) (read w3))
  where
    [w0,_,w2,w3,_,_,_,_,_,_,w10] = words xs

main2 = do
  co <- readFile "input.txt"
  print $ part2 $ lines co

part2 ls = maximum $ map score $ permutations ps
  where
    pqxs = map parse ls
    ps = nub $ map (fst . fst) pqxs
    pqxm = M.fromList $ pqxs ++ [(pq, 0) | p <- ps, pq <- [("me",p), (p,"me")]]
    score ps = sum
      [pqxm M.! (p,q) + pqxm M.! (q,p) | (p,q) <- zip ("me" : ps) (ps ++ ["me"])]
