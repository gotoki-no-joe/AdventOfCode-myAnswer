import qualified Data.Map as M

main = do
    con <- readFile "input.txt"
    let oos = map (\s -> (take 3 s, drop 4 s)) $ lines con
    let answer = compute oos
    print answer

compute :: [(String,String)] -> Int
compute oos = sum $ M.elems ocm
  where
    os = map snd oos
    ocm :: M.Map String Int
    ocm = M.fromList $ ("COM",0) : [ (o, succ $ ocm M.! p) | (p,o) <- oos ]

{-
*Main> main
162439
-}

{-
part2
part1が計算できたということは、COMを根とする木になっていて、循環はない。

YOUからCOMまでの経路を作り、それぞれの距離をとる。
COM6 - B5 - C4 - D3 - E2 - J1 - K0 - YOU
SANからCOMまでも同様にする。
COM4 - B3 - C2 - D1 - I0 - SAN
common prefixを取り去って、共通の D を戻して長さを見ればYOUからSANの距離はわかる。
-}

main2 = do
    con <- readFile "input.txt"
    let oos = map (\s -> (take 3 s, drop 4 s)) $ lines con
    let answer = compute2 oos
    print answer

compute2 :: [(String,String)] -> Int
compute2 oos = length p1 + length p2
  where
    oom :: M.Map String String
    oom = M.fromList $ map (\(a,b) -> (b,a)) oos
    toCom :: String -> [String]
    toCom "COM" = ["COM"]
    toCom o = o : toCom (oom M.! o)
    com2you = reverse $ tail $ toCom "YOU"
    com2san = reverse $ tail $ toCom "SAN"
    (p1,p2) = remPrefix com2you com2san

remPrefix :: Eq a => [a] -> [a] -> ([a],[a])
remPrefix (x:xs) (y:ys) | x == y = remPrefix xs ys
remPrefix xs ys = (xs,ys)

{-
*Main> main2
367
-}
