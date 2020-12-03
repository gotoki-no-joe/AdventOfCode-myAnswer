phase1 = do
  co <- readFile "input.txt"
  let xss = lines co
  print $ compute1 xss
  print $ compute2 xss

compute1 :: [String] -> Int
compute1 xss = length [ () | (xs,i) <- zip (tail xss) [3,6..], xs !! (mod i w) == '#']
  where
    h = length xss
    w = length (head xss)

sample = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

compute2 :: [String] -> Int
compute2 xss = product [sub xss 1, sub xss 3, sub xss 5, sub xss 7, sub xss' 1]
  where
    h = length xss
    w = length (head xss)
    sub xss k = length [ () | (xs,i) <- zip (tail xss) [k,k+k..], xs !! (mod i w) == '#']
    xss' = [ xs | (xs,b) <- zip xss (cycle [True,False]), b ]

{-
*Main> compute1 $ lines sample
7
*Main> compute2 $ lines sample
336
*Main> phase1
247
2983070376
-}
