{-

-}

gen0 =
  [ (x,[a,b,c,d,e,f])
  | a <- [1..6], let x1 = a * 100000
  , b <- [a..9], let x2 = b *  10000 + x1
  , c <- [b..9], let x3 = c *   1000 + x2
  , d <- [c..9], let x4 = d *    100 + x3
  , e <- [d..9], let x5 = e *     10 + x4
  , f <- [e..9], let x  = f          + x5
  , a == b || b == c || c == d || d == e || e == f
  ]

gen1 = takeWhile ((<= 675869).fst) $ dropWhile ((< 172851).fst) gen0

ans1 = length gen1

{-
*Main> ans1
1660
-}

gen0a =
    [ (x,[a,b,c,d,e,f])
    | a <- [1..6], let x1 = a * 100000
    , b <- [a..9], let x2 = b *  10000 + x1
    , c <- [b..9], let x3 = c *   1000 + x2
    , d <- [c..9], let x4 = d *    100 + x3
    , e <- [d..9], let x5 = e *     10 + x4
    , f <- [e..9], let x  = f          + x5
    , a == b && b /= c ||
      b == c && a /= b && c /= d ||
      c == d && b /= c && d /= e ||
      d == e && c /= d && e /= f ||
      e == f && d /= e
    ]
  
gen1a = takeWhile ((<= 675869).fst) $ dropWhile ((< 172851).fst) gen0a
  
ans2 = length gen1a

{-
*Main> ans2
1135
-}
