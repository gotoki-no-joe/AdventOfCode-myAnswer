import Data.Set

main = do
  fi <- readFile "input.txt"
  let ans1 = size $ fromList $ scanl step (0,0) fi
  print ans1
  putStrLn "part2"
  let (sc1,sc2) = fork fi
  let ans2 = size $ union (fromList $ scanl step (0,0) sc1) (fromList $ scanl step (0,0) sc2)
  print ans2

step (x,y) '^' = (x, pred y)
step (x,y) 'v' = (x, succ y)
step (x,y) '<' = (pred x, y)
step (x,y) '>' = (succ x, y)

{-
この展開は面白いな。
同時に処理するのは面倒だから、まずはsplitするか。
-}

{-
fork (a:b:xs) = (a:as,b:bs) where (as,bs) = fork xs
fork [a] = ([a],[])
fork [] = ([],[])
-}

fork (a:xs) = (a:as,bs) where (bs,as) = fork xs
fork [] = ([],[])

{-
*Main> main
2572
part2
2631
-}
