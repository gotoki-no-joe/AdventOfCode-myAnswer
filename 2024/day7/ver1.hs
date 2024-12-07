import Data.List
import qualified Data.IntSet as IS

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part1 ls = sum $ map fst $ filter prop $ map parseLine ls
  where
    prop (w, x:xs) = IS.member w $ foldl step (IS.singleton x) xs
    step is x = IS.union (IS.map (x +) is) (IS.map (x *) is)

parseLine :: String -> (Int,[Int])
parseLine l = (read l1, map read $ words l2)
  where
    (l1,_:l2) = break (':' ==) l

{-
ghci> test1
3749
ghci> main1
4364915411363

大きいな。掛け算あるしそうもなるか。そういえばオーバーフローしなくて助かった。
-}

part2 ls = sum $ map fst $ filter prop $ map parseLine ls
  where
    prop (w, x:xs) = IS.member w $ foldl step (IS.singleton x) xs
    step is x = IS.unions [IS.map (x +) is, IS.map (x *) is, IS.map llx is]
      where
        mag = 10 ^ length (show x)
        llx y = y * mag + x

{-
ghci> test2
11387
ghci> main2
38322057216320

火の玉ストレートだったね。
-}
