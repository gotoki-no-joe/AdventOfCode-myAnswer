{-
コマンドを読み込むのが面倒くさいな。
-}

import Data.List

data CMD a
  = SwapPos Int  Int
  | SwapChr a a
  | RotL    Int
  | RotR    Int
  | RotChr  a
  | Rev     Int  Int
  | Move    Int  Int

exec (SwapPos x y) xs = xs1 ++ c2 : xs2 ++ c1 : xs3
  where
    p1 = min x y
    p2 = max x y
    (xs12,c2:xs3) = splitAt p2 xs
    (xs1,c1:xs2) = splitAt p1 xs12
exec (SwapChr c1 c2) xs = map f xs
  where
    f c
      | c == c1 = c2
      | c == c2 = c1
      | True    = c
exec (RotL n) xs = xs2 ++ xs1
  where
    (xs1,xs2) = splitAt (mod n (length xs)) xs
exec (RotR n) xs = xs2 ++ xs1
  where
    l = length xs
    (xs1,xs2) = splitAt (mod (l-n) l) xs
exec (RotChr c) xs = exec (RotR j) xs
  where
    i = length $ takeWhile (c /=) xs
    j = i + if i >= 4 then 2 else 1
exec (Rev i j) xs = xs1 ++ reverse xs2 ++ xs3
  where
    (xs12,xs3) = splitAt (succ j) xs
    (xs1,xs2) = splitAt i xs12
exec (Move i j) xs = ys1 ++ c : ys2
  where
    (xs1,c:xs2) = splitAt i xs
    (ys1,ys2) = splitAt j (xs1++xs2)

test1 = scanl (flip exec) "abcde" [SwapPos 4 0, SwapChr 'd' 'b', Rev 0 4, RotL 1, Move 1 4, Move 3 0, RotChr 'b', RotChr 'd']

parse :: String -> [CMD Char]
parse = map parseLine . lines

parseLine :: String -> CMD Char
parseLine xs =
  case words xs of
    ("swap":"position":xs1:_:_:xs2:_) -> SwapPos (read xs1) (read xs2)
    ("swap":"letter":xs1:_:_:xs2:_) -> SwapChr (head xs1) (head xs2)
    ("rotate":"left":xs1:_) -> RotL (read xs1)
    ("rotate":"right":xs1:_) -> RotR (read xs1)
    ("rotate":"based":_:_:_:_:xs1:_) -> RotChr (head xs1)
    ("reverse":_:xs1:_:xs2:_) -> Rev (read xs1) (read xs2)
    ("move":_:xs1:_:_:xs2:_) -> Move (read xs1) (read xs2)

testcmds = parse "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d\n"

test2 = foldl (flip exec) "abcde" testcmds

main1 = do
  co <- readFile "input.txt"
  let ans = foldl (flip exec) "abcdefgh" $ parse co
  putStrLn ans

{-
*Main> test1
["abcde","ebcda","edcba","abcde","bcdea","bdeac","abdec","ecabd","decab"]
*Main> test2
"decab"
*Main> main1
gbhcefad
-}

-- 試しにパワーで解いてみるか。

main2 = do
  co <- readFile "input.txt"
  let cmds = parse co
  let ans = findfor "fbgdceah" cmds
  print ans

findfor xs cmds = filter ((xs ==) . doit) $ permutations xs
  where
    doit xs = foldl (flip exec) xs cmds

{-
*Main> main2
["gahedfcb"]
ありゃ、これで解けちゃった。速いなRyzen7
-}
