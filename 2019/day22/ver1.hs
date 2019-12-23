import Data.List

data Shuffle = Deal           -- deal into new stack
             | CutN  Integer  -- cut N
             | DealN Integer  -- deal with increment N

parse :: String -> Shuffle
parse s = case words s of
    "cut" : arg : _ -> CutN (read arg)
    "deal" : "with" : _ : arg : _ -> DealN (read arg)
    "deal" : "into" : _ -> Deal
    _ -> error "parse fail"

sample1, sample2, sample3, sample4 :: [Shuffle]
sample1 = [DealN 7, Deal, Deal]
sample2 = [CutN 6, DealN 7, Deal]
sample3 = [DealN 7, DealN 9, CutN (-2)]
sample4 = [Deal, CutN (-2), DealN 7, CutN 8, CutN (-4), DealN 7, CutN 3, DealN 9, DealN 3, CutN (-1)]

type Deck = [Integer]

naiveInterp :: Integer -> Shuffle -> (Deck -> Deck)
naiveInterp _     Deal     = reverse
naiveInterp base (CutN n)  = \xs -> let (as,bs) = splitAt (fromIntegral $ mod n base) xs in bs ++ as
naiveInterp base (DealN n) = \xs -> map snd $ sort [(j,x) | (i,x) <- zip [0..] xs, let j = i * n `mod` base]

naiveRun :: Integer -> [Shuffle] -> Deck
naiveRun base ss = foldl (flip ($)) [0..base - 1] (map (naiveInterp base) ss)

base1 :: Integer
base1 = 10007

main1 = do
    co <- readFile "input.txt"
    let ans = findIndex (2019 ==) $ naiveRun base1 $ map parse $ lines co
    print ans

-- using modulo

base2, reps :: Integer
base2 = 119315717514047 -- ‭110 1100|1000 0100|0101 1010|1111 0101|0110 0011|0011 1111‬
reps  = 101741582076661 -- ‭101 1100 1000 1000 1000 1110 1101 1011 1100 1010 1111 0101‬

type LFunc = (Integer, Integer)

compose :: Integer -> LFunc -> LFunc -> LFunc
compose modulo (c,d) (a,b) = (a*c `mod` modulo, (b*c + d) `mod` modulo)

apply :: Integer -> LFunc -> Integer -> Integer
apply modulo (a,b) x = (a * x + b) `mod` modulo

forward :: Integer -> Shuffle -> LFunc
forward modulo  Deal     = (modulo-1, modulo-1)
forward modulo (CutN n)  = (1, modulo - n)
forward modulo (DealN n) = (n, 0)

composes :: Integer -> [LFunc] -> LFunc
composes modulo = foldl1 (flip (compose modulo))

forwardTest :: Integer -> [Shuffle] -> Deck
forwardTest modulo ss = map snd $ sort $ zip dests [0..]
  where
    lf = composes modulo $ map (forward modulo) ss
    dests = map (apply modulo lf) [0..modulo - 1]

main1a = do
    co <- readFile "input.txt"
    let ans = apply base1 (composes base1 $ map (forward base1 . parse) $ lines co) 2019
    print ans

-- part2
backward :: Integer -> Shuffle -> LFunc
backward modulo  Deal     = (modulo-1, modulo-1)
backward modulo (CutN n)  = (1, n)
backward modulo (DealN n) = let (1,x,_) = eea n modulo in (x, 0)

-- ax + by = gcd a b を満たす整数 a b とgcd a bを求める
eea :: Integer -> Integer -> (Integer, Integer, Integer) -- (gcd a b, x, y)
eea a b = loop a 1 0 b 0 1
  where
    loop r0 s0 t0 0  _  _  = (r0, s0, t0)
    loop r0 s0 t0 r1 s1 t1 = let (q,r2) = divMod r0 r1 in loop r1 s1 t1 r2 (s0-q*s1) (t0-q*t1)

backcomposes :: Integer -> [LFunc] -> LFunc
backcomposes modulo = foldl1 (compose modulo)

backwardTest :: Integer -> [Shuffle] -> Deck
backwardTest modulo ss = map (apply modulo lf) [0..modulo - 1]
  where
    lf = backcomposes modulo $ map (backward modulo) ss

buildf :: LFunc -> Integer -> LFunc -> LFunc
buildf g 0 _ = g
buildf g r f = buildf g1 (r `div` 2) (compose base2 f f)
  where
    g1 = if even r then g else (compose base2 g f)

main2 = do
    co <- readFile "input.txt"
    let f1 = backcomposes base2 $ map (backward base2 . parse) $ lines co
    let fr = buildf (1,0) reps f1
    let ans = apply base2 fr 2020
    print ans
