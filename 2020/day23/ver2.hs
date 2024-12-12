import Data.Char

import Test.QuickCheck

samplee = "389125467"
input = "784235916"

part1 = post . (!! 100) . iterate step . pre
  where
    post = map intToDigit . take 8 . tail . dropWhile (1 /=) . cycle . flatten
    pre = list2tree . map digitToInt

predd :: Int -> Int
predd 1 = 9
predd k = pred k

step dt = append (append xt3 $ list2tree three) (append xt4 $ Leaf current)
  where
    (xt1,xt2) = split 4 dt
    current : three = flatten xt1
    dest = head $ filter (flip notElem three) $ tail $ iterate predd current
    destPos = length $ takeWhile (dest /=) $ flatten xt2
    (xt3,xt4) = split (succ destPos) xt2

test1 = part1 samplee
main1 = part1 input

{-
ghci> test1
"67384529"
ghci> main1
"53248976"
-}

part2 l = ans
  where
    t0 = list2tree $ [base + d | base <- [0, 9 .. 999990], d <- map digitToInt l] ++ [1000000]
    tZ = iterate step t0 !! 10000000
    ans = take 3 $ dropWhile (1 /=) $ cycle $ flatten tZ

test2 = part2 samplee
main2 = part2 input

-- インタプリタで動かしたらいきなり stack overflow で終わったので、コンパイル実行してみる。
main = print main2

data Tree = Nil | Leaf Int | Node Int Tree Tree deriving Show

-- リストからバランス良く作る
list2tree xs = loop (length xs) xs
  where
--    loop 0 _     = Nil
    loop 0 _ = error "zero sized"
    loop 1 (x:_) = Leaf x
    loop k xs    = Node k (loop k1 xs) (loop (k-k1) $ drop k1 xs)
      where
        k1 = div k 2

-- 切り分ける
split 0 t = (Nil, t)
split p (Node _ lt rt) =
  case compare q 0 of
    EQ -> (lt, rt)
    LT -> (lt1, append lt2 rt)
    GT -> (append lt rt1, rt2)
  where
    q = p - size lt
    (rt1, rt2) = split q rt
    (lt1, lt2) = split p lt
split 1 t@(Leaf _) = (t, Nil)
split p t = error $ unwords [show p, show t]

-- 補助関数
size (Leaf _) = 1
size (Node k _ _) = k
size Nil = 0 -- は普通は出てこないつもり

-- 二つを繋ぐ、バランス調整なし
append Nil rt = rt
append lt Nil = lt
append lt rt = Node (size lt + size rt) lt rt

-- 順に取り出す
flatten t = loop t []
  where
    loop Nil rest = rest
    loop (Leaf x) rest = x : rest
    loop (Node _ lt rt) rest = loop lt $ loop rt rest

propSize (NonEmpty xs) = length xs == size (list2tree xs)

propFlatten (NonEmpty xs) = xs == flatten (list2tree xs)

propAppend (NonEmpty xs) (NonEmpty ys) = xs ++ ys == flatten (append (list2tree xs) (list2tree ys))

propSplit  (NonEmpty xs) (NonEmpty ys) = xs == flatten at && ys == flatten bt
  where
    (at,bt) = split (length xs) $ list2tree (xs ++ ys)