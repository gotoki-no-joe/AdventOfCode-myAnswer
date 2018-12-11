import Data.List.Split
import Data.Either
import Data.List

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let wss = map divideHyp ls
  let ans1 = length $ filter isTLP wss
  print ans1
  let ans2 = length $ filter hasSSL wss
  print ans2

divideHyp :: String -> ([String],[String])
divideHyp cs = partitionEithers $ inner cs where
  inner "" = []
  inner cs
    | notElem '[' cs = [Left cs]
    | True =
        let
          (as,'[':bs) = break ('[' ==) cs
          (ps,']':qs) = break (']' ==) bs
        in
          Left as : Right ps : inner qs

hasABBA :: String -> Bool
hasABBA (a:b:c:d:xs) = a == d && b == c && a /= b || hasABBA (b:c:d:xs)
hasABBA _ = False

isTLP :: ([String],[String]) -> Bool
isTLP (as,bs) = any hasABBA as && all (not . hasABBA) bs

getABA :: String -> [(Char,Char)]
getABA (a:b:c:xs)
  | a == c && a /= b = (a,b) : rest
  | True = rest
  where rest = getABA (b:c:xs)
getABA _ = []

ab2ba (x,y) = (y,x)

hasSSL :: ([String],[String]) -> Bool
hasSSL (as,bs) = not $ null ist where
  as1 = concatMap getABA as
  bs1 = concatMap getABA bs
  ist = intersect as1 (map ab2ba bs1)

{-
*Main> main
115
231
-}
