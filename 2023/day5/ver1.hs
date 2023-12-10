import Data.List.Split
import qualified Data.IntMap as IM
import Data.List

part1 fn = do
  seedsL : mapsL <- splitWhen null . lines <$> readFile fn
  let seeds = map read $ tail $ words $ head seedsL
  let maps = map (map (map read . words) . tail) mapsL
  print $ solve1 seeds maps

solve1 :: [Int] -> [[[Int]]] -> Int
solve1 seeds maps = minimum $ map (\x -> foldl (flip ($)) x funs) seeds
  where
    funs = map (readMap . makeMap) maps

    makeMap xss = IM.fromList [(b,(c,a)) | a:b:c:_ <- xss]
    readMap im x =
      case IM.lookupLE x im of
        Just (b,(c,a)) | x <= b + c -> a + x - b
        _ -> x

{-
ghci> part1 "sample.txt"
35
ghci> part1 "input.txt"
251346198
-}

{-
[a,b) 半開区間で範囲を表現する。
まず、入力の列は半開区間の列で受け取るので、順序と重なりを正規化する。 regSortedSpans
次に、写像も、定義域の半開区間と、地域のベースという対で表しておく。ここに多分重なりはないと信じよう。 theMaps

そうしたら、前者を後者に当てはまる区間ずつ適用して、バラバラになった区間をまたソートして正規化する、で１ステップ。


-}

part2 fn = do
  seedsL : mapsL <- splitWhen null . lines <$> readFile fn
  let seeds = sort $ map (\(a:b:_) -> (a, a + b)) $ chunksOf 2 $ map read $ tail $ words $ head seedsL
  let maps = map (map (map read . words) . tail) mapsL
  print $ solve2 seeds maps

regSortedSpans :: [(Int, Int)] -> [(Int, Int)]
regSortedSpans ((a,b):abs) = loop a b abs
  where
    loop a b [] = [(a,b)]
    loop a b ((c,d):cds)
      | b < c = (a,b) : loop c d cds
      | True  = loop a (max b d) cds

applyMap2Spans :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)]
applyMap2Spans abs cdes = regSortedSpans $ sort $ loop abs cdes
  where
    loop ababs@((a,b):abs) cdecdes@((c,d,e):cdes)
      | b < c = (a,b) : loop abs cdecdes
      | a < c = (a,c) : loop ((c, b):abs) cdecdes
      | a < d, b < d = (e + a - c, e + b - c) : loop abs cdecdes
      | a < d = (e + a - c, e + d - c) : loop ((d, b):abs) cdes
      | otherwise = loop ababs cdes
--    loop abs [] = abs
--    loop [] _   = []
    loop abs _  = abs

solve2 :: [(Int, Int)] -> [[[Int]]] -> Int
solve2 seeds maps = fst $ head $ foldl applyMap2Spans (regSortedSpans seeds) theMaps
  where
    theMaps = map (\m1 -> sort [(b, b + c, a) | a:b:c:_ <- m1]) maps

{-
ghci> part2 "sample.txt"
46
ghci> part2 "input.txt"
72263011

インフルでしばらく中断、ゆっくり落ち着いて作ったら地味に正解できた。やれやれだ。
-}
