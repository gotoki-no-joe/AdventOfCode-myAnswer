data Tree = Node [Tree] [Int] deriving Show

main = do
  fi <- readFile "input.txt"
  let ns = map read $ words fi
  let ([t],[]) = buildTrees 1 ns
--  print t
  print (metasum t)
  putStrLn "part2"
  print (getValue t)

buildTrees :: Int   -> -- 長さ
              [Int] -> -- 元データ
              ([Tree],[Int]) -- 木リストと残り
buildTrees 0 ns = ([],ns)
buildTrees n (c:m:ns) = (t:ts, ns3) where
  t = Node children meta
  (children,ns1) = buildTrees c ns
  (meta,ns2) = splitAt m ns1
  (ts,ns3) = buildTrees (pred n) ns2

testdata = buildTrees 1 [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]

metasum :: Tree -> Int
metasum (Node cs meta) = sum (map metasum cs ++ meta)

test1 = metasum $ head $ fst testdata

{-
必要になる度に計算するように書くと計算しに行ってしまうので、
子ノードの値のリストを変数に割り当てるように書く必要がある。
-}

getValue :: Tree -> Int
getValue (Node [] meta) = sum meta
getValue (Node cs meta) = sum $ map f meta where
  vals = map getValue cs
  f i = if 0 < i && i <= length cs then vals !! (i-1) else 0

test2 = getValue $ head $ fst testdata

{-
*Main> main
42768
part2
34348
-}
