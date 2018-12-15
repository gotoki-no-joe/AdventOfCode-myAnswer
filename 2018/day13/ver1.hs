{-
ステージは文字単位の2次元配列にして素早くアクセスしたい。
カートはY,X座標でソートかけて整理して、
tick内の移動を順に行い、移動前と移動後の両方に衝突していないことを
確認しつつ進める。
全部終わったらtickの切り替えで整列しなおす。
カートの他の内部状態は、
進行方向と次に旋回する方向。
-}

import Debug.Trace
import qualified Data.Array.IArray as A
import Control.Monad
import Data.List (sort)

test0 = exec "test0.txt"
test1 = exec "test1.txt"
main = exec "input.txt"

exec fn = do
  fi <- readFile fn
  let ls = lines fi
  let carts = readCarts ls
  let field = buildField ls
  print (A.bounds field)
  let ans1 = run field carts
  print ans1
  putStrLn "part2"
  let ans2 = run2 field carts
  print ans2

type Vec = (Int,Int)
type Cart = (Vec,Vec,Int)
type Field = A.Array (Int,Int) Char

readCarts :: [String] -> [Cart]
readCarts ls =
  [ ((y,x), d c, 0)
  | (l,y) <- zip ls [0..]
  , (c,x) <- zip l  [0..]
  , elem c "^v><"
  ]
  where
    d '^' = (-1,0)
    d 'v' = ( 1,0)
    d '<' = (0,-1)
    d '>' = (0, 1)

buildField :: [String] -> Field
buildField ls = A.array ((0,0),(h,w)) assoc where
  h = length ls - 1
  w = length (head ls) - 1
  assoc =
    [ ((y,x),f c)
    | (l,y) <- zip ls [0..]
    , (c,x) <- zip l  [0..]
    ]
  f '^' = '|'
  f 'v' = '|'
  f '<' = '-'
  f '>' = '-'
  f c   = c

-- 正常ならカートの位置、事故ったら現場座標
tick :: Field -> [Cart] -> Either Vec [Cart]
tick field carts = step [] (sort carts) where
  step :: [Cart] -> [Cart] -> Either Vec [Cart]
  step moved [] = Right moved
  step moved (c:carts)
    | crash p moved = Left p
    | crash p carts = Left p
    | otherwise = step (cart:moved) carts
    where cart@(p,_,_) = single field c

single :: A.Array (Int,Int) Char -> Cart -> Cart
single field (pos,vel,d) = -- trace (show pos) $
  case (field A.! pos, vel) of
    ('|', (_,0)) -> (add pos vel, vel, d)
    ('-', (0,_)) -> (add pos vel, vel, d)
    ('\\',(b,a)) -> let v' = (a,b) in (add pos v', v', d)
    ('/', (b,a)) -> let v' = (-a,-b) in (add pos v', v', d)
    ('+', v    ) -> let (v',d') = cross v d in (add pos v', v', d')
    (fld, _    ) -> error (unwords [show fld, show pos, show vel, show d])

cross v 1 = (v, 2) -- 今回直進
cross (y,x) 0 = ((-x,y), 1) -- 今回左折
cross (y,x) 2 = ((x,-y), 0) -- 今回右折

crash p carts = any (p ==) [ q | (q,_,_) <- carts ]

add (x,y) (a,b) = (x+a,y+b)

{- (x,y) 表記
\
(1,0) -> (0,1)
(-1,0) -> (0,-1)
(0,1) -> (1,0)
(0,-1) -> (-1,0)

/
(1,0) -> (0,-1)
(-1,0) -> (0,1)
(0,1) -> (-1,0)
(0,-1) -> (1,0)

左折
(1,0) -> (0,-1)
(0,-1) -> (-1,0)
(-1,0) -> (0,1)
(0,1) -> (1,0)

右折
(1,0) -> (0,1)
(0,1) -> (-1,0)
(-1,0) -> (0,-1)
(0,-1) -> (1,0)
-}

-- 事故るまで走らせる
run field carts = case tick field carts of
  Left p       -> p
  Right carts1 -> run field carts1

---- part2 ----

-- 事故ったカートを即座に撤去して、最後の1台になるまで走らせる。
-- …交差点で3台や4台が同時に事故ることあるのか？

-- 正常ならカートの位置、事故ったら消えていく
tick2 :: Field -> [Cart] -> [Cart]
tick2 field carts = step [] (sort carts) where
  step :: [Cart] -> [Cart] -> [Cart]
  step moved [] = moved
  step moved (c:carts)
    | crash p moved = step (remove p moved) carts
    | crash p carts = step moved (remove p carts)
    | otherwise = step (cart:moved) carts
    where cart@(p,_,_) = single field c

remove p carts = [ cart | cart@(q,_,_) <- carts, p /= q ]

-- 最後の1台になるまで走らせる。
run2 field carts
  | singleton carts1 = head carts1
  | null carts1 = error "no car left."
  | otherwise = run2 field carts1
  where carts1 = tick2 field carts

singleton [_] = True
singleton _ = False

{-
*Main> test0
((0,0),(6,0))
(3,0)
part2
*** Exception: no car left.
*Main> test1
((0,0),(5,12))
(3,7)
part2
*** Exception: no car left.
*Main> main
((0,0),(149,149))
(90,124)
part2
((88,145),(1,0),0)

Y,Xの順であることに注意。
-}
