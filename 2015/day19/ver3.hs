{-# Language Strict #-}

import Debug.Trace
import qualified Data.Sequence as Q

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let rules = map parse $ take (length ls - 2) ls
  let initial = Q.fromList $ last ls
  let ans1 = compute1 rules initial
  print ans1
  let ans2 = compute2 rules initial
  print ans2

type Rule = (Q.Seq Char,Q.Seq Char)

process :: [Rule] -> Q.Seq Char -> [Q.Seq Char]
process rules molecule =
  [ ys Q.>< b Q.>< (Q.drop (Q.length a) zs)
  | i <- [0..Q.length molecule]
  , let (ys,zs) = (Q.zip (Q.inits molecule) (Q.tails molecule)) `Q.index` i
  , (a,b) <- rules
  , isPrefixOf a zs
  ]

isPrefixOf xs ys = Q.length xs <= Q.length ys && and [ Q.index xs i == Q.index ys i | i <- [0..length xs - 1] ]

compute1 rules initial = length $ process rules initial

parse cs = (Q.fromList a, Q.fromList b) where
  (a,' ':'=':'>':' ':b) = break (' ' ==) cs

testrule = map parse ["H => HO", "H => OH", "O => HH"]

test1 = compute1 testrule $ Q.fromList "HOH"
test2 = compute1 testrule $ Q.fromList "HOHOHO"
test3 = process [parse "H => OO"] $ Q.fromList "H2O"

testrule2 = map parse
  ["e => H"
  ,"e => O"
  ,"H => HO"
  ,"H => OH"
  ,"O => HH"]

test4 = compute2 testrule2 $ Q.fromList "HOH"
test5 = compute2 testrule2 $ Q.fromList "HOHOHO"

compute2 rules dest = recur 0 dest (error "not found") where
  electron = Q.fromList "e"
  revrules = map (\(a,b) -> (b,a)) $ filter ((electron /=).fst) rules
  firstmolecs = map snd $ filter ((electron ==).fst) rules
  recur n molecule rest
    | elem molecule firstmolecs = succ n
    | otherwise = foldr (recur n1) rest molecules where
        n1 = succ n
        molecules = process revrules molecule
