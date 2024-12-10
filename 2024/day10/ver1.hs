{-
そこから到達できる 9 の座標のリスト、を持つような配列を作ればstaticにできるかな？
-}

import Data.Array
import Data.List
import Debug.Trace

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = (ans, sum ans)
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    arr = listArray bnds $ concat ls
    to9 = listArray bnds $ map to9f $ range bnds
    to9f ij =
      case arr ! ij of
        '9' -> [ij]
        d   -> concat [to9 ! pq | pq <- neighbors ij, succ d == arr ! pq]
    neighbors (i,j) = filter (inRange bnds) [(pred i,j),(succ i,j),(i,pred j),(i,succ j)]
    ans = [length $ nub $ to9 ! ij| (ij,'0') <- assocs arr]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 ls = (ans, sum ans)
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    arr = listArray bnds $ concat ls
    to9 = listArray bnds $ map to9f $ range bnds
    to9f ij =
      case arr ! ij of
        '9' -> [ij]
        d   -> concat [to9 ! pq | pq <- neighbors ij, succ d == arr ! pq]
    neighbors (i,j) = filter (inRange bnds) [(pred i,j),(succ i,j),(i,pred j),(i,succ j)]
    ans = [length $ to9 ! ij| (ij,'0') <- assocs arr]
