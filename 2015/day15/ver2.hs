-- 2022-11-18

import Data.Array
import Data.Ix
import Data.List

part1 list recipes =
  maximum
    [ product $ map (max 0) $ init $ foldl1' (zipWith (+)) $ zipWith (map . (*)) re list
    | re <- recipes ]

mkRecipe num = recipeA ! (num, 100)
  where
    bnds = ((1,0), (num,100))
    recipeA = array bnds [(kw,recipeF kw) | kw <- range bnds]
    recipeF (1,w) = [[w]]
    recipeF (k,0) = [replicate k 0]
    recipeF (k,w) = [x : ys | x <- [0..w], ys <- recipeA ! (pred k, w - x)]

parse :: String -> [Int]
parse xs = map read (map init [cap,dur,fvr,tex] ++ [cal])
  where
    [_,_,cap,_,dur,_,fvr,_,tex,_,cal] = words xs

main1 = do
  co <- readFile "input.txt"
  let list = map parse $ lines co
  let recipes = mkRecipe $ length list
  print $ part1 list recipes
  print $ part2 list recipes

part2 list recipes =
  maximum
    [ product $ map (max 0) $ init vals
    | re <- recipes
    , let vals = foldl1' (zipWith (+)) $ zipWith (\r ps -> map (r *) ps) re list
    , last vals == 500
    ]
