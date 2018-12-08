import Data.List (tails)

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let nices = filter nice ls
  print (length nices)
  let betters = filter better ls
  print (length betters)

nice :: String -> Bool
nice str = c1 && c2 && c3 where
  c1 = threeup (filter (flip elem "aeiou") str)
  ps = zip str (tail str)
  c2 = any (uncurry (==)) ps
  c3 = null $ filter (flip elem [('a','b'),('c','d'),('p','q'),('x','y')]) ps

threeup [] = False
threeup [_] = False
threeup [_,_] = False
threeup _ = True

better :: String -> Bool
better str = c1 && c2 where
  c2 = or $ zipWith (==) str (drop 2 str)
  c1 = not $ null [ () | (a:b:cs) <- tails str
                       , (p:q:rs) <- tails cs
                       , a == p, q == b ]

{-
  c1 = not $ null [ () | (a:b:cs) <- tails str, appear a b cs ]

appear p q rs = not $ null [ () | (a:b:cs) <- tails rs, p == a, q == b ]
-}

{-
*Main> main
255
55
-}
