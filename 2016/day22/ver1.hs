import Data.List

getData = do
  co <- readFile "input.txt"
  return $ map parseLine1 $ drop 2 $ lines co

-- Used, Avail
parseLine1 :: String -> (Int,Int)
parseLine1 xs = (read $ init xs1, read $ init xs2)
  where
    (_:_:xs1:xs2:_) = words xs

answer1 uas = sum
  [ viable1 ua1 ua2 + viable1 ua2 ua1
  | (ua1:uas1) <- tails uas
  , ua2 <- uas1]

viable1 (u,a) (v,b)
  | u > 0, b >= u = 1
  | otherwise     = 0

{-
*Main> uas <- getData
*Main> answer1 uas
864
-}
