import qualified Data.Map as M
import qualified Data.IntSet as IS

compute1 :: String -> Int
compute1 co = m M.! "root"
  where
    m = M.fromList $ map (parse . words) $ lines co
    parse [key, num] = (init key, read num)
    parse [key, m1, op:_, m2] = (init key, funof op (m M.! m1) (m M.! m2))
    parse _ = error "never"

funof '+' = (+)
funof '-' = (-)
funof '*' = (*)
funof '/' = div
funof _ = error "never"

test1 = readFile  "test.txt" >>= print . compute1
main1 = readFile "input.txt" >>= print . compute1

compute2 :: String -> Int
compute2 co = m M.! (parent M.! "humn", "humn")
  where
    wls = map words $ lines co
    parent = M.fromList $ concatMap parseParent wls
    parseParent [key, m1, _, m2] = [(m1, init key),(m2, init key)]
    parseParent _ = []
-- m M.! (w,wFrom) : wをwFromから見たときの値
    m = M.fromList $ concatMap parse wls
    parse (w:ws) =
      case ws of
        [num] -> [((w1,wP), read num)]
        [wL,_,wR] | w1 == "root" -> [((w1,wL), m M.! (wR,w1)),((w1,wR), m M.! (wL,w1))]
        [wL,'+':_,wR] -> [((w1,wP), m M.! (wL,w1) + m M.! (wR,w1))
                         ,((w1,wL), m M.! (wP,w1) - m M.! (wR,w1))        -- w1(wL) = wP - wR
                         ,((w1,wR), m M.! (wP,w1) - m M.! (wL,w1))]       -- w1(wR) = wP - wL
        [wL,'-':_,wR] -> [((w1,wP), m M.! (wL,w1) - m M.! (wR,w1))
                         ,((w1,wL), m M.! (wP,w1) + m M.! (wR,w1))        -- w1(wL) = wP + wR
                         ,((w1,wR), m M.! (wL,w1) - m M.! (wP,w1))]       -- w1(wR) = wL - wP
        [wL,'*':_,wR] -> [((w1,wP), m M.! (wL,w1) * m M.! (wR,w1))
                         ,((w1,wL), m M.! (wP,w1) `div` m M.! (wR,w1))    -- w1(wL) = wP / wR
                         ,((w1,wR), m M.! (wP,w1) `div` m M.! (wL,w1))]   -- w1(wR) = wP / wL
        [wL,'/':_,wR] -> [((w1,wP), m M.! (wL,w1) `div` m M.! (wR,w1))
                         ,((w1,wL), m M.! (wP,w1) * m M.! (wR,w1))        -- w1(wL) = wP * wR
                         ,((w1,wR), m M.! (wL,w1) `div` m M.! (wP,w1))]   -- w1(wR) = wL / wP
        _ -> error $ "never" ++ show (w:ws)
      where
        w1 = init w
        wP = parent M.! w1
    parse _ = error "never"

test2 = readFile  "test.txt" >>= print . compute2
main2 = readFile "input.txt" >>= print . compute2

compute3 :: String -> IS.IntSet
compute3 co = m M.! (parent M.! "humn", "humn")
  where
    wls = map words $ lines co
    parent = M.fromList $ concatMap parseParent wls
    parseParent [key, m1, _, m2] = [(m1, init key),(m2, init key)]
    parseParent _ = []
-- m M.! (w,wFrom) : wをwFromから見たときの値
    m = M.fromList $ concatMap parse wls
    parse (w:ws) =
      case ws of
        [num] -> [((w1,wP), IS.singleton $ read num)]
        [wL,_,wR] | w1 == "root" -> [((w1,wL), m M.! (wR,w1)),((w1,wR), m M.! (wL,w1))]
        [wL,'+':_,wR] -> [((w1,wP), opapp (+) (m M.! (wL,w1)) (m M.! (wR,w1)))
                         ,((w1,wL), opapp (-) (m M.! (wP,w1)) (m M.! (wR,w1)))        -- w1(wL) = wP - wR
                         ,((w1,wR), opapp (-) (m M.! (wP,w1)) (m M.! (wL,w1)))]       -- w1(wR) = wP - wL
        [wL,'-':_,wR] -> [((w1,wP), opapp (-) (m M.! (wL,w1)) (m M.! (wR,w1)))
                         ,((w1,wL), opapp (+) (m M.! (wP,w1)) (m M.! (wR,w1)))        -- w1(wL) = wP + wR
                         ,((w1,wR), opapp (-) (m M.! (wL,w1)) (m M.! (wP,w1)))]       -- w1(wR) = wL - wP
        [wL,'*':_,wR] -> [((w1,wP), opapp (*) (m M.! (wL,w1)) (m M.! (wR,w1)))
                         ,((w1,wL), opMulInv (m M.! (wP,w1)) (m M.! (wR,w1)))    -- w1(wL) = wP / wR
                         ,((w1,wR), opMulInv (m M.! (wP,w1)) (m M.! (wL,w1)))]   -- w1(wR) = wP / wL
        [wL,'/':_,wR] -> [((w1,wP), opapp div (m M.! (wL,w1)) (m M.! (wR,w1)))
                         ,((w1,wL), opDivInv1 (m M.! (wP,w1)) (m M.! (wR,w1)))        -- w1(wL) = wP * wR
                         ,((w1,wR), opDivInv2 (m M.! (wL,w1)) (m M.! (wP,w1)))]   -- w1(wR) = wL / wP
        _ -> error $ "never" ++ show (w:ws)
      where
        w1 = init w
        wP = parent M.! w1
    parse _ = error "never"

opapp op s t = IS.fromList [op x y | x <- IS.elems s, y <- IS.elems t]
opMulInv s t = IS.fromList [q | x <- IS.elems s, y <- IS.elems t, let (q,r) = divMod x y, r == 0]
opDivInv1 s t = IS.fromList [z | x <- IS.elems s, y <- IS.elems t, z <- [x * y .. x * y + pred y]]
opDivInv2 s t = IS.fromList [z | x <- IS.elems s, y <- IS.elems t, z <- [divrup (succ y) (succ x) .. div y x]]
divrup x y = negate $ div (negate x) y

test3 = readFile  "test.txt" >>= print . compute3
main3 = readFile "input.txt" >>= print . compute3
