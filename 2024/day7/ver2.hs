import qualified Data.IntSet as IS

import System.CPUTime

runner i f = do
  ls <- lines <$> readFile i
  print $ f $ map parse ls

parse :: String -> (Int, [Int])
parse l = (read as, map read $ words bs)
  where
    (as,_:bs) = break (':' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [(Int,[Int])] -> Int
part1 rxss = sum [ r | (r,xs) <- rxss, elem r $ compute xs]
  where
    compute (x:xs) = foldl step [x] xs
    step ys x = map (x +) ys ++ map (x *) ys

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [(Int,[Int])] -> Int
part2 rxss = sum [ r | (r,xs) <- rxss, elem r $ compute r xs]
  where
    compute r (x:xs) = foldl (step r) [x] xs
    step r ys x = filter (r >=) $ map (x +) ys ++ map (x *) ys ++ map (ll x) ys
--    ll x y = read $ show y ++ show x
    ll x y = y * mag + x
      where
        mag = 10 ^ length (show x)

test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a

part2a :: [(Int,[Int])] -> Int
part2a rxss = sum [ r | (r,xs) <- rxss, IS.member r $ compute r xs]
  where
    compute r (x:xs) = foldl (step r) (IS.singleton x) xs
    step r ys x = fst $ IS.split (succ r) $ IS.unions
      [IS.mapMonotonic (x +) ys, IS.mapMonotonic (x *) ys, IS.mapMonotonic (ll x) ys]
    ll x y = y * mag + x
      where
        mag = 10 ^ length (show x)

timeit action = do
  t1 <- getCPUTime
  action
  t2 <- getCPUTime
  print $ t2 - t1
