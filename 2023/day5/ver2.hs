import Data.List.Split
import qualified Data.IntMap as IM

import Data.Maybe
import Data.List

import Debug.Trace

import Test.QuickCheck

runner i f = do
  sl : ls <- lines <$> readFile i
  let seeds = map read $ words $ drop 7 sl
      mss = map (map (map read . words) . tail) $ wordsBy null ls
  print $ f seeds mss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

ms2map ms = IM.union opens cloze
  where
    opens = IM.fromList           [(dom, ran - dom) | ran:dom:_ <- ms]
    cloze = IM.fromList $ (0,0) : [(dom + wid, 0)  | _:dom:wid:_ <- ms]

apply im x = x + o
  where
    Just (_,o) = IM.lookupLE x im

part1 :: [Int] -> [[[Int]]] -> Int
part1 seeds mss = minimum $ map applyAll seeds
  where
    applyAll x = foldl (flip ($)) x $ map (apply . ms2map) mss

-- part2

newtype SpanSet = SS {getIM :: IM.IntMap Bool}
  deriving Show -- デバッグ用

type Span = (Int,Int)

emptySS :: SpanSet
emptySS = SS IM.empty

-- 集合ssに整数xが含まれるか
isIn :: SpanSet -> Int -> Bool
isIn ss x = maybe False snd $ IM.lookupLE x $ getIM ss

-- 集合ssに区間[u,v)を追加
addSpan :: SpanSet -> Span -> SpanSet
addSpan ss (u,v) = SS $
  (if isIn ss (pred u) then id else IM.insert u True) $
  (if isIn ss v then id else IM.insert v False) $
  IM.union is1 is2
  where
    (is1,_) = IM.split u $ getIM ss
    (_,is2) = IM.split v $ getIM ss

valid :: SpanSet -> Bool
valid (SS is) = even (IM.size is) && and (zipWith (==) (IM.elems is) (cycle [True, False]))

prop_addSpan1 (NonNegative x) (Positive w) = valid s1
  where
    s1 = addSpan emptySS (x, x + w)

prop_addSpan2 ss (NonNegative x) (Positive w) = valid ss ==> valid (addSpan ss (x, x + w))

instance Arbitrary SpanSet where
  arbitrary = sized $ \n -> do
    xs <- map getNonNegative <$> vectorOf n arbitrary
    ys <- map getPositive <$> vectorOf n arbitrary
    return $ foldl (\ss (x, y) -> addSpan ss (x, x + y)) (SS IM.empty) $ zip xs ys

-- 集合ssのx未満の範囲
under :: SpanSet -> Int -> SpanSet
under ss x
  | isIn ss (pred x) = SS $ IM.insert x False is1
  | otherwise = SS is1
  where
    (is1, _) = IM.split x $ getIM ss

prop_under ss x = valid $ under ss x

-- 集合ssのx以上の範囲
over :: SpanSet -> Int -> SpanSet
over ss x
  | isIn ss x = SS $ IM.insert x True is1
  | otherwise = SS is1
  where
    (_, is1) = IM.split x $ getIM ss

prop_over ss x = valid $ over ss x

-- 集合ssにran dom widな写像を適用する
-- 残されるdom未満の部分およびdom+wid以上の部分と、
-- dom以上dom+wid未満の各区間をranに写した結果のリストを返す
apply2ss :: SpanSet -> [Int] -> (SpanSet, [Span])
apply2ss ss (ran:dom:wid:_) = (rest, uvs)
  where
    domu = dom + wid
    rest = SS $ IM.union (getIM $ under ss dom) (getIM $ over ss domu)
    ofs = ran - dom
    uvs = map l2t $ chunksOf 2 $ map (ofs +) $ IM.keys $ getIM $ flip under domu $ over ss dom

l2t [u,v] = (u,v)

-- ひとつの写像は (ran:dom:wid:_) のリストで構成される
-- これを全て適用する（区間は交差しない前提で考える）
applyMap :: SpanSet -> [[Int]] -> SpanSet
applyMap is0 ms = foldl addSpan isZ $ concat uvss
  where
    (isZ, uvss) = mapAccumL apply2ss is0 ms

part2 :: [Int] -> [[[Int]]] -> Int
part2 seeds mss = x
  where
    is0 = foldl addSpan emptySS $ map (mkSpan . l2t) $ chunksOf 2 seeds
    SS isZ = foldl applyMap is0 mss
    (x,True) = IM.findMin isZ
    mkSpan (x,w) = (x, x + pred w)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2
