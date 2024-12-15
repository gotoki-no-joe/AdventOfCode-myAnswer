import Data.List
import Data.List.Split
import Data.Char

import qualified Data.IntSet as IS
import Data.Array
-- import qualified Data.IntMap as IM

runner i f = do
  nss <- map (map read . wordsBy (not . isDigit)) . lines <$> readFile i
  let (nss1, _:nss2) = break null nss
  print $ f nss1 nss2

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [[Int]] -> [[Int]] -> Int
part1 rules = sum . map getCenter . filter (validate rules)

getCenter xs = xs !! div (length xs) 2

validate rules update = and
  [ valid i j
  | l:r:_ <- rules
  , let i = elemIndex l update
  , let j = elemIndex r update]
  where
    valid (Just a) (Just b) = a < b
    valid _ _ = True

test1a = runner "sample.txt" part1a
main1a = runner "input.txt" part1a

part1a :: [[Int]] -> [[Int]] -> Int
part1a rules = sum . map getCenter . filter (validate2 ra)
  where
    ra = fmap IS.fromList $ accumArray (flip (:)) [] (1,99) [(r, l) | l:r:_ <- rules]

validate2 ra update = loop IS.empty update
  where
    loop _ [] = True
    loop prohibited (x:xs) = IS.notMember x prohibited && loop (IS.union prohibited $ ra ! x) xs

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

-- 探索

part2 :: [[Int]] -> [[Int]] -> Int
part2 rules = sum . map (getCenter . makeCorrect ra) . filter (not . validate2 ra)
  where
    ra = fmap IS.fromList $ accumArray (flip (:)) [] (1,99) [(r, l) | l:r:_ <- rules]

makeCorrect ra update = head $ loop $ IS.fromList update
  where
    loop upd
      | IS.null upd = [[]]
      | otherwise =
        [ p : qs
        | p <- IS.elems upd, IS.disjoint upd $ ra ! p
        , qs <- loop $ IS.delete p upd]

{-
がんばってトポロジカルソートを手書きしたけど意味なかった。

part2 :: [[Int]] -> [[Int]] -> Int
part2 rules updates =  sum
  [ sorted !! div (length update) 2
  | update <- updates
  , not $ validate2 ra update
  , let sorted = makeCorrect update
  ]
  where
    ra = fmap IS.fromList $ accumArray (flip (:)) [] (1,99) [(r, l) | l:r:_ <- rules]
    makeCorrect update = loop im0
      where
        rules1 = [rule | rule@(l:r:_) <- rules, elem l update, elem r update]
        im0 = IM.fromListWith (+) $ [(p,0) | p <- update] ++ [(r,1) | _:r:_ <- rules1]
        locks = IM.fromListWith (++) [(l,[r]) | l:r:_ <- rules1]
        loop im
          | IM.null im = []
          | otherwise  =
          case [p |(p,0) <- IM.assocs im] of
            [p] -> p : loop (foldr (uncurry (IM.insertWith (+))) (IM.delete p im) [(r,-1) | r <- locks IM.! p])
            _ -> error "something wrong"
-}
