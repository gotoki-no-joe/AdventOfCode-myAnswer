-- 2022-12-2

main1 = do
  co <- readFile "input.txt"
  print $ sum $ map (score1 . map head . words) $ lines co
  print $ sum $ map (score2 . map head . words) $ lines co

{-
A: Rock, B:Paper, C:Scissors
X: Rock, Y:Paper, Z:Scissors
   1       2        3        : Shape Point
0,3,6 : lose, draw, win point
-}

score :: [Char] -> Int
score [a, 'X'] = 1 + case a of { 'A' -> 3; 'B' -> 0; 'C' -> 6; _ -> 999 }
score [a, 'Y'] = 2 + case a of { 'A' -> 6; 'B' -> 3; 'C' -> 0; _ -> 999 }
score [a, 'Z'] = 3 + case a of { 'A' -> 0; 'B' -> 6; 'C' -> 3; _ -> 999 }
score _ = error "hoge"

score1 :: [Char] -> Int
score1 [a, 'X'] = 1 + scoresub a (3,0,6) -- X=Rock 対 a での game point
score1 [a, 'Y'] = 2 + scoresub a (6,3,0)
score1 [a, 'Z'] = 3 + scoresub a (0,6,3)
score1 _ = error "hoge"

scoresub 'A' (a,b,c) = a
scoresub 'B' (a,b,c) = b
scoresub 'C' (a,b,c) = c
scoresub _ _ = error "hoge"

{-
A: Rock, B:Paper, C:Scissors
X: Lose, Y:Draw,  Z:Win
   0       3        6    : Game Point
-}

score2 :: [Char] -> Int
score2 [a, 'X'] = 0 + scoresub a (3,1,2) -- aにLoseするための手のShape Point
score2 [a, 'Y'] = 3 + scoresub a (1,2,3)
score2 [a, 'Z'] = 6 + scoresub a (2,3,1)
score2 _ = error "hoge"
