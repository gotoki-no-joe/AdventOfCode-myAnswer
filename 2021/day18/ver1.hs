import Text.Read
import Control.Applicative
import Control.Monad
import Data.List

-- その数はペアであり、中身は数かペアである

data SFNum = SFPair SFNum SFNum | SFNum Int

instance Show SFNum where
  showsPrec _ (SFNum n) = shows n
  showsPrec _ (SFPair a b) = showChar '[' . shows a . showChar ',' . shows b . showChar ']'

instance Read SFNum where
  readPrec = readint +++ readpair

readint :: ReadPrec SFNum
readint = SFNum <$> readPrec

readpair :: ReadPrec SFNum
readpair = do
  c <- get
  when (c /= '[') pfail
  l <- readPrec
  c <- get
  when (c /= ',') pfail
  r <- readPrec
  c <- get
  when (c /= ']') pfail
  return $ SFPair l r

test1 :: [SFNum]
test1 = map read ["[1,2]","[[1,2],3]","[9,[8,7]]","[[1,9],[8,5]]","[[[[1,2],[3,4]],[[5,6],[7,8]]],9]","[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]","[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"]

{-
*Main> mapM_ print test1
[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
-}

magnitude :: SFNum -> Int
magnitude (SFNum n) = n
magnitude (SFPair a b) = magnitude a * 3 + magnitude b * 2

{-
explodeの実装は難儀だぞ。「すぐ左」「すぐ右」のregular numberを見つけないといけない。
深さ5以上にあるregular numberのみからなるpairで一番左にあるものを見つけて、
見つからなかったらそのままで、見つかったとき、それ自体を0にしつつ、左と右の直近のregular numberに足しこむ数をも返す。
戻ってきたものが「見つからなかった」なら、左を試す。
「見つかった」かつ、左右の数の足しこみが終わっていないなら、それもする、は、
左の部分木で見つかったときに右の部分木の一番左に足しこむ、右の部分木で見つかったときに左の部分木の一番右に足しこむ、はできるけど、
逆のときは...もういちど木をいじくり直すだけか、同じか。できそうだ。
-}

explode :: SFNum -> Maybe SFNum
explode sfn = (\(t,_,_) -> t) <$> loop 0 sfn
  where
    loop dep (SFPair (SFNum a) (SFNum b)) | dep >= 4 = Just (SFNum 0, Just a, Just b)
    loop dep (SFPair l r) =
      case (loop (succ dep) l, loop (succ dep) r) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just (t, Nothing, mbb)) -> Just (SFPair l t, Nothing, mbb)
        (Nothing, Just (t, Just a , mbb)) -> Just (SFPair (addRight a l) t, Nothing, mbb)
        (Just (t, mba, Nothing), _      ) -> Just (SFPair t r, mba, Nothing)
        (Just (t, mba, Just b ), _      ) -> Just (SFPair t (addLeft  b r), mba, Nothing)
    loop dep (SFNum _) = Nothing

addRight :: Int -> SFNum -> SFNum
addRight x (SFNum v) = SFNum (x+v)
addRight x (SFPair a b) = SFPair a (addRight x b)

addLeft :: Int -> SFNum -> SFNum
addLeft x (SFNum v) = SFNum (x+v)
addLeft x (SFPair a b) = SFPair (addLeft x a) b

test2 :: [Maybe SFNum]
test2 = map (explode . read) ["[[[[[9,8],1],2],3],4]","[7,[6,[5,[4,[3,2]]]]]","[[6,[5,[4,[3,2]]]],1]","[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]","[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"]

{-
*Main> mapM_ print test2
Just [[[[0,9],2],3],4]
Just [7,[6,[5,[7,0]]]]
Just [[6,[5,[7,0]]],3]
Just [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
Just [[3,[2,[8,0]]],[9,[5,[7,0]]]]
-}

split :: SFNum -> Maybe SFNum
split (SFNum n) | n >= 10 = let h = div n 2 in Just $ SFPair (SFNum h) (SFNum $ n-h)
split (SFPair a b) =
  case (split a, split b) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just b1) -> Just $ SFPair a b1
    (Just a1, _)       -> Just $ SFPair a1 b
split _ = Nothing

addSF :: SFNum -> SFNum -> SFNum
addSF a b = reduce $ SFPair a b

reduce :: SFNum -> SFNum
reduce n =
  case explode n of
    Just n1 -> reduce n1
    Nothing -> case split n of
      Nothing -> n
      Just n2 -> reduce n2

test3 = addSF (read "[[[[4,3],4],4],[7,[[8,4],9]]]") (read "[1,1]")

test4 = map (foldl1 addSF . map read) [["[1,1]","[2,2]","[3,3]","[4,4]"],["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]"],["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]","[6,6]"]]

{-
*Main> mapM_ print test4
[[[[1,1],[2,2]],[3,3]],[4,4]]
[[[[3,0],[5,3]],[4,4]],[5,5]]
[[[[5,0],[7,4]],[5,5]],[6,6]]
-}

test5 = foldl1 addSF $ map read $ lines "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"

{-
*Main> test5
[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
-}

sample6 = map read $ lines "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

test6 = foldl1 addSF sample6

{-
*Main> test6
[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
*Main> magnitude test6
4140
-}

run1 = readFile "input.txt" >>= print . magnitude . foldl1 addSF . map read . lines

{-
*Main> run1
4137
-}

compute2 ns = maximum [magnitude a | n:ns1 <- tails ns, m <- ns1, a <- [addSF n m, addSF m n]]

run2 = readFile "input.txt" >>= print . compute2 . map read . lines

{-
*Main> compute2 sample6
3993
*Main> run2
4573
-}
