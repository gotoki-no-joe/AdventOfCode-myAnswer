import Data.List.Split
import Data.List
import Control.Applicative
import Data.Maybe
import Data.Bits

runner i f = do
  lss <- wordsBy null . lines <$> readFile i
  print $ f lss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 lss = sum $ map score lss
  where
    score ls = fromJust $ (100 *) <$> sub ls <|> sub (transpose ls)
    sub ls = uncurry (loop 1) $ splitAt 1 ls
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | and $ zipWith (==) as bbs = Just k
      | otherwise = loop (succ k) (b : as) bs

part1a lss = sum $ map score lss
  where
    score ls = fromJust $ (100 *) <$> sub ls <|> sub (transpose ls)
    sub ls = uncurry (loop 1) $ splitAt 1 $ map l2b ls
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | and $ zipWith (==) as bbs = Just k
      | otherwise = loop (succ k) (b : as) bs

l2b :: String -> Int
l2b "" = 0
l2b ('.':cs) = l2b cs .<<. 1
l2b ('#':cs) = l2b cs .<<. 1 .|. 1

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 lss = sum $ map score2 lss

score2 :: [String] -> Int
score2 ls = fromJust $ (100 *) <$> scoresub2 ls <|> scoresub2 (transpose ls)

scoresub2 ls = loop 1 as bs
  where
    (as,bs) = splitAt 1 $ map l2b ls
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | (1 ==) $ sum $ map popCount $ zipWith xor as bbs = Just k
      | otherwise = loop (succ k) (b : as) bs

diff0 as bs = and $ zipWith (==) as bs
diff1 as bs = 1 == sum (zipWith diffCount as bs)
  where
    diffCount xs ys = length $ filter id $ zipWith (/=) xs ys

diff1a as bs = 1 == sum (zipWith diffCount as bs)
  where
    diffCount x y = popCount $ xor x y

part12 pre prop lss = sum $ map score lss
  where
    score ls = fromJust $ (100 *) <$> sub ls <|> sub (transpose ls)
    sub ls = uncurry (loop 1) $ splitAt 1 $ map pre ls
    loop _ _ [] = Nothing
    loop k as bbs@(b:bs)
      | prop as bbs = Just k
      | otherwise = loop (succ k) (b : as) bs

test1a = runner "sample.txt" $ part12 id diff0
main1a = runner "input.txt"  $ part12 id diff0
main1b = runner "input.txt"  $ part12 l2b diff0

test2a = runner "sample.txt" $ part12 id diff1
main2a = runner "input.txt"  $ part12 id diff1
main2b = runner "input.txt"  $ part12 l2b diff1a
