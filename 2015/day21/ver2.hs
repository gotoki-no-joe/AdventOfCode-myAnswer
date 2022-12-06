import Data.List

rings = [(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3),(0,0,0)]

cags = sort
  [ weapon `add` armor `add` ring1 `add` ring2
  | weapon <- [(8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0)]
  , armor <- [(0,0,0),(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,5)]
  , (ring1:rs) <- tails rings
  , ring2 <- if null rs then [ring1] else rs
  ]
add (a,b,c) (d,e,f) = (a+d,b+e,c+f)

-- @gotoki_no_joe
divrup x y = negate $ div (negate x) y

myHP = 100
enHP = 103
enAP = 9
enGP = 2

win myAP myGP = divrup myHP (max 1 (enAP - myGP)) >= divrup enHP (max 1 (myAP - enGP))

part1 = head $ filter (\(c,a,g) -> win a g) cags

part2 = head $ filter (\(c,a,g) -> not $ win a g) $ reverse cags
