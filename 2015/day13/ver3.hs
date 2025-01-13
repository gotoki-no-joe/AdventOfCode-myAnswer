import Data.List
import qualified Data.Map as M

parse :: String -> ((String,String), Int)
parse l = (pq, x)
  where
    ws = words l
    pq = (ws !! 0, init $ ws !! 10)
    x = (if ws !! 2 == "gain" then id else negate) (read $ ws !! 3)

runner i f = readFile i >>= print . f . map parse . lines

part1 pqxs = maximum $ map score $ permutations ps
  where
    (p1:ps) = nub $ map (fst . fst) pqxs
    pqxm = M.fromListWith (+) $ concat [[pqx, ((q,p),x)] | pqx@((p,q),x) <- pqxs]
    score ps = sum [pqxm M.! (p,q) | (p,q) <- zip (p1 : ps) (ps ++ [p1])]

test1 = runner "test.txt" part1
main1 = runner "input.txt" part1

part2 pqxs = maximum $ map score $ permutations ps
  where
    ps = nub $ map (fst . fst) pqxs
    pqxm = M.fromListWith (+) $ concat [[pqx, ((q,p),x)] | pqx@((p,q),x) <- pqxs]
    score (p1:ps) = sum [pqxm M.! (p,q) | (p,q) <- zip (p1:ps) ps]

main2 = runner "input.txt" part2
