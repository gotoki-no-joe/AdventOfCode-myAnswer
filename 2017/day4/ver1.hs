import Data.List

main = do
  fi <- readFile "input.txt"
  let ps = map words $ lines fi
  let ps1 = filter valid ps
  print (length ps1)
  let ps2 = filter valid $ map (map sort) ps1
  print (length ps2)

valid [] = True
valid (x:xs) = notElem x xs && valid xs

{-
*Main> main
466
251

今までで一番簡単だったね。
-}
