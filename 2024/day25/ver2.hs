import Data.List.Split
import Data.List

runner i f = do
  ents <- wordsBy null . lines <$> readFile i
  print $ f ents

check ents = (length ents, all (head ls ==) ls, nub ents == ents)
  where
    ls = map length ents

part1 ents = length
  [ ()
  | e <- ents, head (head e) == '.'
  , let is = process e
  , js <- locks
  , all (7 >=) $ zipWith (+) is js
  ]
  where
    locks = [process e | e <- ents, head (head e) == '#']
    process = map (length . filter ('#' ==)) . transpose

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
