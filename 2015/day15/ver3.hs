import Data.List.Split
import Data.Array
import Data.List

parse :: String -> [Int]
parse = map read . wordsBy (flip notElem ('-':['0'..'9']))

runner i f = readFile i >>= print . f . map parse . lines

recipe :: Int -- 残りの材料種類
       -> Int -- 残りの配合量
       -> [[Int]] -- レシピのリスト
-- 材料が最後なら残量全部それ
recipe 1 w = [[w]]
-- 配合がいっぱいなら、残りの材料は全部使わない
recipe k 0 = [replicate k 0]
-- 先頭の材料をx = 0～w使い、残りの材料の使い方は再帰的に求めて組み合わせる
recipe k w = [x:ys | x <- [0..w], ys <- recipe (pred k) (w - x)]

mkRecipe k w = recipeA ! (k, w)
  where
    bnds = ((1,0), (k,w))
    recipeA = listArray bnds $ map recipeF $ range bnds
    recipeF (1, w) = [[w]]
    recipeF (k, 0) = [replicate k 0]
    recipeF (k, w) = [x : ys | x <- [0..w], ys <- recipeA ! (pred k, w - x)]

part1x list = maximum $ map value $ mkRecipe (length list) 100
  where
    value ws = product $ map (max 0 . sum) $ transpose $
               zipWith (\w ps -> map (w *) $ init ps) ws list

part1 list = maximum
    [ product $ init vals
    | ws <- mkRecipe (length list) 100
    , let vals = values ws ]
  where
    tlist = transpose list
    values ws = map (max 0 . sum . zipWith (*) ws) tlist

test1 = runner "test.txt" part1
main1 = runner "input.txt" part1

part2 list = maximum
    [ product $ init vals
    | ws <- mkRecipe (length list) 100
    , let vals = values ws
    , last vals == 500 ] -- ココ
  where
    tlist = transpose list
    values ws = map (max 0 . sum . zipWith (*) ws) tlist

test2 = runner "test.txt" part2
main2 = runner "input.txt" part2
