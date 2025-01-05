import Data.List.Split

data R = R {red :: Int, green :: Int, blue :: Int}
  deriving Show

type Game = (Int, [R])

parse :: String -> Game
parse l = (gameID, recs)
  where
    l1 : l2 : _ = splitOn ": " l
    gameID = read $ drop 5 l1
    recs = map mkRec $ splitOn "; " l2
    mkRec l3 = foldr ($) (R 0 0 0)
      [ setCol col $ read num
      | l3 <- splitOn ", " l3
      , let num : col : _ = words l3 ]
    setCol ('r':_) x rec = rec { red = x }
    setCol ('g':_) x rec = rec { green = x }
    setCol ('b':_) x rec = rec { blue = x }

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

runner i f = do
  gs <- map parse . lines <$> readFile i
  print $ f gs

part1 :: [Game] -> Int
part1 gs = sum [i | (i, recs) <- gs, all prop recs]
  where
    prop r = red r <= 12 && green r <= 13 && blue r <= 14

{-
ghci> test1
8
ghci> main1
2268
-}

part2 gs = sum
  [ r * g * b
  | (_,recs) <- gs
  , let r = maximum $ map red   recs
  , let g = maximum $ map green recs
  , let b = maximum $ map blue  recs ]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
2286
ghci> main2
63542
-}
