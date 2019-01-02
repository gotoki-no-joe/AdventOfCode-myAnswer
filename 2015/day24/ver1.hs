import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List

test = qe1st $ head $ compute $ [1..5] ++ [7..11]

type TheMap = M.IntMap [S.IntSet]

-- 初期状態
map0 :: TheMap
map0 = M.singleton 0 [S.empty]

qe1st (xs,_,_) = product $ S.elems xs

compute :: [Int] -> [(S.IntSet, S.IntSet, S.IntSet)]
compute ws = ans where
  w3 = sum ws `div` 3
  -- addをwsに関して一通り行う
  map9 = foldr add map0 ws
-- w3を作る組み合わせから、disjointな3つの組み合わせを見つける。
-- 長さ、QEの順で小さい順に整列してからなら、先頭が答えになる。
  ans =
    [ (xs,ys,zs)
    | let xss0 = map9 M.! w3
    , let xss = map (\ (_,_,xs) -> xs ) $
                sort [ (S.size xs, product (S.elems xs), xs) | xs <- xss0 ]
    , xs:yss <- tails xss
    , ys:zss <- tails yss
    , disjoint xs ys
    , zs <- zss
    , disjoint xs zs, disjoint ys zs
    ]
-- mapに対して、ある重さwを足した重さを全て追加する。
-- (ただし、wが既にあるものは却下。は不要)
  add :: Int -> TheMap -> TheMap
  add w1 m1 = M.unionWith (++) m1 m2 where
    m2 = M.fromAscList
      [ (w2, map (S.insert w1) xss)
      | (w,xss) <- M.assocs m1
      , let w2 = w + w1
      , w2 <= w3 ]

disjoint xs ys = S.null $ S.intersection xs ys

{-
和がw3になるようなすべての組み合わせを見つけ出すためにはどうしたらいいのかしら。
いかにもDPが必要そうな状況だけど。

部分選択について、それが作る重さをキーとして記録する、つまり

IntMap [IntSet]

ここで member xs (m ! w) のとき、sum (elems xs) == w

というMapを作っていけばいいのかしら。
重さは全てuniqueという有り難い条件を満たしているので、
同じモノを再度加算する愚は容易に回避できる。
-}

part1 = do
  fi <- readFile "input.txt"
  let ws = map read $ lines fi
  let ans = head $ compute ws
  print ans
  print (qe1st ans)

compute2 :: [Int] -> [S.IntSet]
compute2 ws = ans where
  w4 = sum ws `div` 4
  -- addをwsに関して一通り行う
  map9 = foldr add map0 ws
-- w3を作る組み合わせから、disjointな3つの組み合わせを見つける。
-- 長さ、QEの順で小さい順に整列してからなら、先頭が答えになる。
  ans =
    [ xs
    | let xss0 = map9 M.! w4
    , let xss = map (\ (_,_,xs) -> xs ) $
                sort [ (S.size xs, product (S.elems xs), xs) | xs <- xss0 ]
    , xs:yss <- tails xss
    , ys:zss <- tails yss
    , disjoint xs ys
    , zs:wss <- tails zss
    , disjoint xs zs, disjoint ys zs
    , ws <- wss
    , disjoint xs ws, disjoint ys ws, disjoint zs ws
    ]
-- mapに対して、ある重さwを足した重さを全て追加する。
-- (ただし、wが既にあるものは却下。は不要)
  add :: Int -> TheMap -> TheMap
  add w1 m1 = M.unionWith (++) m1 m2 where
    m2 = M.fromAscList
      [ (w2, map (S.insert w1) xss)
      | (w,xss) <- M.assocs m1
      , let w2 = w + w1
      , w2 <= w4 ]

test2 = compute2 $ [1..5] ++ [7..11]

part2 = do
  fi <- readFile "input.txt"
  let ws = map read $ lines fi
  let ans = compute2 ws
  print (head ans)
  print (product $ S.elems $ head ans)

{-
*Main> test
99
*Main> part1
(fromList [1,83,103,107,109,113],fromList [3,13,61,73,79,89,97,101],fromList [5,11,17,19,23,29,31,41,43,47,53,59,67,71])
11266889531
*Main> test2
[fromList [4,11],fromList [4,11],fromList [5,10]]
*Main> part2
fromList [1,61,103,109,113]
77387711

あまり盛り上がらない。
-}
