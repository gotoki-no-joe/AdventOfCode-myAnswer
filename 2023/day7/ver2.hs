import Data.Array
import Data.List

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> (String, Int)
parse l = (h, read ds)
  where
    h:ds:_ = words l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

-- part1
part1 hbs =
    sum $ zipWith mul [1 ..] $
    sort [(score $ cardcnt is, is, b) |(h,b) <- hbs, let is = h2is dic h]
  where
    dic = zip "23456789TJQKA" [2 ..]

    cardcnt is =
      sortBy (flip compare) $ elems $
      accumArray (+) 0 (2,14) [(i,1) | i <- is]

mul r (_,_,b) = r * b

h2is dic h = [i | c <- h, let Just i = lookup c dic]

score ks =
  case ks of
    (5:_)   -> 7 -- five card
    (4:_)   -> 6 -- four card
    (3:2:_) -> 5 -- full house
    (3:_)   -> 4 -- three card
    (2:2:_) -> 3 -- two pair
    (2:_)   -> 2 -- one pair
    _       -> 1 -- buta

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

-- part2
part2 hbs =
    sum $ zipWith mul [1 ..] $
    sort [(score $ cardcnt is, is, b) |(h,b) <- hbs, let is = h2is dic h]
  where
    dic = zip "J23456789TQKA" [1 ..]

    cardcnt is = cj+k1 : ks
      where
        k1:ks   = sortBy (flip compare) cnts
        cj:cnts = elems $ accumArray (+) 0 (1,13) [(i,1) | i <- is]

{-
ghci> test2
5905
ghci> main2
248652697
-}
