{-
ひっかけな言い方をしているが、
錠については*の、鍵については.の個数を数えることで、「同じ数」のそれを素早く発見できる。
-}

import Data.List
import Data.List.Split

import qualified Data.Map as M

runner i f = do
  ents <- wordsBy null . lines <$> readFile i
  print $ f ents

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

checkSize ents = (ls, length ls, all (head ls ==) ls, length (nub ls) == length ls)
  where
    ls = map length ents

part1 ents = sum
  [ cnt
  | e <- ents, head (head e) == '.'
  , let is = map (count '#') $ transpose $ init $ tail e
  , (js,cnt) <- M.assocs locks
  , all (5 >=) $ zipWith (+) is js
  ]
  where
    locks = M.fromListWith (+) [(map (count '#') $ transpose $ init $ tail e, 1) | e <- ents, head (head e) == '#']
--    keys = M.fromListWith (+) [(map (count '.') $ transpose $ init $ tail e, 1) | e <- ents, head (head e) == '.']


count x = length . filter (x ==)

{-
ghci> test1
([7,7,7,7,7],5,True)
ghci> main1
7,7,7,7],500,True)

重複するの？

そして、fitというのはぴったりではなくて、重なりはしない、なのか。
とすると、足して5を越えない、をしないといけない感じ。総当たりでいいかそれ。
-}
