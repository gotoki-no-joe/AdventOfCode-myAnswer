import Data.List
import Data.List.Split

main = do
  fi <- readFile "input.txt"
  let sues = map parse $ lines fi
  let ans1 = compute1 sues
  print ans1
  let ans2 = compute2 sues
  print ans2

parse :: String -> [(String,Int)]
parse cs = [ (key, read value)
           | [key,value] <- map (splitOn ": ") $ splitOn ", " cs1 ] where
  cs1 = drop 2 $ dropWhile (':' /=) cs

compute1 :: [[(String,Int)]] -> [Int]
compute1 aunts = [ i | (i,a) <- zip [1..] aunts, matchOne a ]

matchOne :: [(String,Int)] -> Bool
matchOne kvs = all (checkSingle kvs) sensedData where
  sensedData = [("children", 3), ("cats",7),("samoyeds", 2)
               ,("pomeranians", 3),("akitas", 0),("vizslas", 0)
               ,("goldfish", 5),("trees", 3),("cars", 2),("perfumes", 1)]

checkSingle :: [(String,Int)] -> (String,Int) -> Bool
checkSingle kvs (k,v) =
  case lookup k kvs of
    Nothing -> True
    Just x  -> x == v

compute2 :: [[(String,Int)]] -> [Int]
compute2 aunts = [ i | (i,a) <- zip [1..] aunts, matchOne2 a ]

matchOne2 :: [(String,Int)] -> Bool
matchOne2 kvs = all (checkSingle2 kvs) sensedData where
  sensedData = [("children", 3,EQ), ("cats",7,GT),("samoyeds", 2,EQ)
               ,("pomeranians", 3,LT),("akitas", 0,EQ),("vizslas", 0,EQ)
               ,("goldfish", 5,LT),("trees", 3,GT),("cars", 2,EQ),("perfumes", 1,EQ)]

checkSingle2 :: [(String,Int)] -> (String,Int,Ordering) -> Bool
checkSingle2 kvs (k,v,ing) =
  case lookup k kvs of
    Nothing -> True
    Just x  -> compare x v == ing

{-
*Main> main
[40]
[241]

Orderingみたいな値がない言語だとあっさり面倒そうだな。
-}
