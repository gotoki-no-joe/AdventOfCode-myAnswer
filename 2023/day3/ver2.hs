import Data.Array
import Data.Char
import Data.List

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      ls1 = pad (replicate (w + 2) '.') $ map (pad '.') ls
      fld = listArray ((0,0),(succ h,succ w)) $ concat ls1
  print $ f h w fld

pad :: a -> [a] -> [a]
pad x xs = x : xs ++ [x]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

-- アクセサを挟む方法

runner1 i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls
  print $ f h w (access '.' fld)

access outv arr pos
  | inRange (bounds arr) pos = arr ! pos
  | otherwise = outv

part1 h w fld = sum
  [ read $ map (fld !) ijs
  | ij <- range ((1,1),(h, w))
  , isDigit $ fld ! ij
  , not $ isDigit $ fld ! left ij
  , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
  , any (symbol . (fld !)) $ concatMap neighbors ijs ]

left  (i,j) = (i, pred j)
right (i,j) = (i, succ j)

neighbors (i,j) = [(p,q) | p <- [pred i .. succ i], q <- [pred j .. succ j], (i,j) /= (p,q)]

symbol c = not (isDigit c) && c /= '.'

{-
ghci> test1
4361
ghci> main1
526404
-}

part2 h w fld = sum [a * b | [a,b] <- elems gnums]
  where
    gnums = accumArray (flip (:)) [] ((1,1),(h,w))
      [ (gearPos, val)
      | ij <- range ((1,1),(h, w))
      , isDigit $ fld ! ij
      , not $ isDigit $ fld ! left ij
      , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
      , let val = read $ map (fld !) ijs
      , gearPos <- nub $ filter (('*' ==) . (fld !)) $ concatMap neighbors ijs ]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
467835
ghci> main2
84399773
-}

neighborss ijs =
    (i, pred jL) : (i, succ jR) :
    [(i1,j) | i1 <- [pred i, succ i], j <- [pred jL .. succ jR]]
  where
    (i,jL) = head ijs
    (_,jR) = last ijs

part1a h w fld = sum
  [ read $ map (fld !) ijs
  | ij <- range ((1,1),(h, w))
  , isDigit $ fld ! ij
  , not $ isDigit $ fld ! left ij
  , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
  , any (symbol . (fld !)) $ neighborss ijs ]

part2a h w fld = sum [a * b | [a,b] <- elems gnums]
  where
    gnums = accumArray (flip (:)) [] ((1,1),(h,w))
      [ (gearPos, val)
      | ij <- range ((1,1),(h, w))
      , isDigit $ fld ! ij
      , not $ isDigit $ fld ! left ij
      , let ijs = takeWhile (isDigit . (fld !)) $ iterate right ij
      , let val = read $ map (fld !) ijs
      , gearPos <- filter (('*' ==) . (fld !)) $ neighborss ijs ]

test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a
