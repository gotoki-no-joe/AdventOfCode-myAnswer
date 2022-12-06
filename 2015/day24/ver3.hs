-- 2022-11-26
import Data.Bits
import Data.List
import Data.Array
import qualified Data.IntMap as IM

test1 = compute1 $ [1..5] ++ [7..11]

compute1 :: [Int] -> Int
compute1 xs = ans
  where
-- 目標重量
    w3 = div (sum xs) 3
-- 包みの個数
    num = length xs
    im = foldl' step (IM.singleton 0 [0]) $ zip xs [0..]
--    step im (x, i) = IM.unionWith (++) im $ IM.mapKeysMonotonic (x +) $ IM.map (map (bit i .|.)) im
    step im (x, i) = IM.unionWith (++) im $ IM.fromAscList $
      [(w1, map (bit i .|.) bs) | (w,bs) <- IM.assocs im, let w1 = w + x, w1 <= w3]
    bs = im IM.! w3 :: [Int]
    cands = sort [(popCount b, qe, b) | b <- bs, let qe = product [x | (i,x) <- zip [0..pred num] xs, testBit b i]]
    ans = head
      [ qe
      | (_,qe,b1):cands1 <- tails cands
      , (_,_ ,b2):cands2 <- tails cands1, b1 .&. b2 == 0, let b12 = b1 .|. b2
      , (_,_ ,b3)        <-       cands2, b12 .&. b3 == 0 -- , b12 .|. b3 == 2^num - 1 ちょうどになるに決まっとる
      ]

main1 = do
  co <- readFile "input.txt"
  print $ compute1 $ map read $ lines co

-- 候補 211,830個は結構な数だな。

compute2 :: [Int] -> Int
compute2 xs = ans
  where
-- 目標重量
    w4 = div (sum xs) 4
-- 包みの個数
    num = length xs
    im = foldl' step (IM.singleton 0 [0]) $ zip xs [0..]
--    step im (x, i) = IM.unionWith (++) im $ IM.mapKeysMonotonic (x +) $ IM.map (map (bit i .|.)) im
    step im (x, i) = IM.unionWith (++) im $ IM.fromAscList $
      [(w1, map (bit i .|.) bs) | (w,bs) <- IM.assocs im, let w1 = w + x, w1 <= w4]
    bs = im IM.! w4 :: [Int]
    cands = sort [(popCount b, qe, b) | b <- bs, let qe = product [x | (i,x) <- zip [0..pred num] xs, testBit b i]]
    ans = head
      [ qe
      | (_,qe,b1):cands1 <- tails cands
      , (_,_ ,b2):cands2 <- tails cands1, b1   .&. b2 == 0, let b12 = b1 .|. b2
      , (_,_ ,b3):cands3 <- tails cands2, b12  .&. b3 == 0, let b123 = b12 .|. b3
      , (_,_ ,b4)        <-       cands3, b123 .&. b4 == 0 --, b123 .|. b4 == 2^num - 1
      ]

test2 = compute2 $ [1..5] ++ [7..11]

main2 = do
  co <- readFile "input.txt"
  print $ compute2 $ map read $ lines co
