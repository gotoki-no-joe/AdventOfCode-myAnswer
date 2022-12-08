{-
DP的なことをする感じかな。
いやもっと、簡単にできる。ただし、pureでするにはどうしたら、という感じ。
-}
import Data.Array
import Data.List
import Data.Function

test1 = body1 "test.txt"
main1 = body1 "input.txt"

body1 fn = do
  ls <- lines <$> readFile fn
  let h = length ls
  let w = length $ head ls
  let ans = part1 h w ls
  print ans

part1 h w dss = length . filter id . elems $ arr
  where
    tdss = transpose dss
    arr = accumArray (||) False ((1,1),(h,w)) $
      [((i,j), True) | (i,ds) <- zip [1..] dss , (j,True) <- zip [1..] (check ds)] ++
      [((i,j), True) | (i,ds) <- zip [1..] dss , (j,True) <- zip [w, pred w..] (check $ reverse ds)] ++
      [((i,j), True) | (j,ds) <- zip [1..] tdss, (i,True) <- zip [1..] (check ds)] ++
      [((i,j), True) | (j,ds) <- zip [1..] tdss, (i,True) <- zip [h, pred h..] (check $ reverse ds)]

{-
count :: Ord a => [a] -> Int
count xs = succ . length . takeWhile id . zipWith (<) xs . tail $ xs
-}

check :: (Enum a, Ord a) => [a] -> [Bool]
check xs = zipWith (>) xs $ scanl max (pred $ head xs) xs

{-
左から見て何人までは見えるか、を数えることができる。
reverseしたりtransposeをreverseしたりすると、それぞれの向きで数えられる。
それで見える人をarrayで塗って、塗られた人を数えると。

ギャーここまで書いて間違ってた。
countの修正で立ち直れるな。
-}

{-
パート２は、賢く高速に数える方法が思いつかない。ないんじゃないかな。
一応、あっちからこっちに移し替える奴で、同時に双方向の値を計測できる。ランダムアクセス配列はいらない。
-}

test2 = body2 "test.txt"
main2 = body2 "input.txt"

body2 fn = do
  ls <- lines <$> readFile fn
  let h = length ls
  let w = length $ head ls
  let ans = part2 h w ls
  print ans

part2 h w dss = maximumBy (compare `on` snd) . assocs $ arr
  where
    tdss = transpose dss
    arr = accumArray (*) 1 ((1,1),(h,w)) $
      [((i,j), s) | (i,ds) <- zip [1..] dss , (j,s) <- zip [1..] $ part2sub [] ds] ++
      [((i,j), s) | (j,ds) <- zip [1..] tdss, (i,s) <- zip [1..] $ part2sub [] ds]

part2sub ys [] = []
part2sub ys (x:xs) = count ys * count xs : part2sub (x:ys) xs
  where
    count zs = case span (x >) zs of
      (as,[]) -> length as
      (as,_ ) -> succ $ length as
