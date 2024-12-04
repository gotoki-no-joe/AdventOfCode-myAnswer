import Data.Array
runner i f = do
  ss <- lines <$> readFile i
  let ans = f ss
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part1 :: [String] -> Int
part1 ss = length
  [ ()
  | ij <- range bnds, acc ij == 'X'
  , d <- [(a,b) | a <- [-1 .. 1], b <- [-1 .. 1], a /= 0 || b /= 0]
  , let ij1 = add ij  d, acc ij1 == 'M'
  , let ij2 = add ij1 d, acc ij2 == 'A'
  , let ij3 = add ij2 d, acc ij3 == 'S']
  where
    h = length ss
    w = length (head ss)
    bnds = ((1,1),(h,w))
    arr = listArray bnds $ concat ss
    acc ij
      | inRange bnds ij = arr ! ij
      | otherwise = '.'
    add (a,b) (c,d) = (a+c,b+d)

part2 :: [String] -> Int
part2 ss = length
  [ ()
  | ij <- range bnds, acc ij == 'A'
  , prop (acc $ add ij (-1,-1)) (acc $ add ij (1,1))
  , prop (acc $ add ij (-1,1)) (acc $ add ij (1,-1))
  ]
  where
    h = length ss
    w = length (head ss)
    bnds = ((1,1),(h,w))
    arr = listArray bnds $ concat ss
    acc ij
      | inRange bnds ij = arr ! ij
      | otherwise = '.'
    add (a,b) (c,d) = (a+c,b+d)
    prop 'M' 'S' = True
    prop 'S' 'M' = True
    prop _ _ = False

