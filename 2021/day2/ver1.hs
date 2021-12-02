main1 fn = do
  co <- readFile fn
  print $ compute1 co

compute1 :: String -> (Int,Int,Int)
compute1 inp = (x,y,x*y)
  where
    (x,y) = foldl step1 (0,0) $ lines inp

step1 (x,y) li =
  case (a, read b) of
    ("forward", dx) -> (x+dx, y)
    ("down"   , dy) -> (x, y+dy)
    ("up"     , dy) -> (x, y-dy)
    _ -> undefined
  where
    [a,b] = words li

test1 = main1 "sample.txt"

run1 = main1 "input.txt"

{-
*Main> test1
(15,10,150)
*Main> run1
(1868,1090,2036120)
-}

main2 fn = do
  co <- readFile fn
  print $ compute2 co

compute2 :: String -> (Int,Int,Int)
compute2 inp = (x,y,x*y)
  where
    (x,y,_) = foldl step2 (0,0,0) $ lines inp

step2 (x,y,aim) li =
  case (a, read b) of
    ("forward", dx) -> (x+dx, y+dx*aim, aim)
    ("down"   , dy) -> (x, y, aim+dy)
    ("up"     , dy) -> (x, y, aim-dy)
    _ -> undefined
  where
    [a,b] = words li

test2 = main2 "sample.txt"

run2 = main2 "input.txt"

{-
*Main> test2
(15,60,900)
*Main> run2
(1868,1078987,2015547716)
-}
