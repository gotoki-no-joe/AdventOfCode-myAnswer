
{-
0～2の座標のタプルをうにょうにょするだけか？
-}

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let code = compute1 ls
  print code
  let code2 = compute2 ls
  print code2

step (a,b) 'U'
  | 0 < b = (a, pred b)
step (a,b) 'D'
  | b < 2 = (a, succ b)
step (a,b) 'L'
  | 0 < a = (pred a, b)
step (a,b) 'R'
  | a < 2 = (succ a, b)
step ab _ = ab

compute1 = map t2i . tail . scanl run (1,1)

t2i (a,b) = b*3+a+1

-- run ab xs = last $ scanl step ab xs
run ab xs = foldl step ab xs

{-
うわーこう来たかぁ。
はみ出しは(2,2)とのマンハッタン距離で判定する。
文字への変換は余りのあるテーブルでやるのが
手っ取り早いかな。
-}

step2 (a,b) d
  | abs (a'-2) + abs (b'-2) <= 2 = (a',b')
  | otherwise = (a,b)
  where
    (a',b') = case d of
      'U' -> (a, pred b)
      'D' -> (a, succ b)
      'L' -> (pred a, b)
      'R' -> (succ a, b)

ab2c (a,b) = chars !! b !! a where
  chars = ["..1..", ".234.", "56789", ".ABC.", "..D.."]

-- run2 ab xs = last $ scanl step2 ab xs
run2 ab xs = foldl step2 ab xs

compute2 = map ab2c . tail . scanl run2 (0,2)

{-
*Main> main
[1,8,8,4,3]
"67BB9"
-}
