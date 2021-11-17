{-
これは、人ならだいたいでわかることを、厳密にコード化するのは面倒なパターンだね。

まず、一番上の行を走査して '|' を探す。そこから下に進み始める。
'|'とアルファベットと'-'に遭遇した時には、さらに下に進む。アルファベットのときは出力する。
'+'に遭遇したら、横のどちらかに線があるはずなので探し、そちらに進み始める。

そういうことか。そしてどこにも辿れなくなったら終了。サンプルは左端だけど、input.txtにはそれが見当たらないので条件はそれではないようだ。

-}

import Data.Array

testmap = readMap "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ \n                \n"

type Map = Array (Int,Int) Char

readMap :: String -> Map
readMap str = listArray ((1,1),(h,w)) $ concat ls
  where
    ls = lines str
    h = length ls
    w = length (head ls)

type Dir = (Int,Int)

deltaU, deltaR, deltaD, deltaL :: Dir
deltaU = (-1,0)
deltaR = (0,1)
deltaD = (1,0)
deltaL = (0,-1)

turn :: Dir -> Dir
turn d
  | d == deltaU = deltaR
  | d == deltaD = deltaR
  | otherwise   = deltaD

type Pos = (Int,Int)

findStart :: Map -> Pos
findStart m = head [p | x <- [1..snd $ snd $ bounds m], let p = (1,x), m ! p == '|']

run :: Map -> Pos -> Dir -> String
run m p@(y,x) d@(dy,dx)
--  | y < 0 || x < 0 || fst $ snd $ bounds m < y || snd $ snd $ bounds m < x == " Run Out Map"
  | c == '+' = if m ! pt1 /= ' ' then run m pt1 (ty,tx) else if m ! pt2 /= ' ' then run m pt2 (-ty,-tx) else error ("no way " ++ show p)
  | 'A' <= c, c <= 'Z' = c : run m pd d -- 文字を踏んで先に進む
  | c == ' ' = " space end" ++ show p
  | True = run m pd d
  where
    c = m ! p
    pd = (y+dy,x+dx)
    (ty,tx) = turn d
    pt1 = (y+ty,x+tx)
    pt2 = (y-ty,x-tx)

main1 = do
  co <- readFile "input.txt"
  let m = readMap co
  putStrLn $ run m (findStart m) deltaD

{-
*Main> run testmap (findStart  testmap) deltaD
"ABCDEF space end(4,1)"
*Main> main1
ITSZCJNMUO space end(99,38)
-}

run2 :: Map -> Pos -> Dir -> String
run2 m p@(y,x) d@(dy,dx)
  | c == '+' = if m ! pt1 /= ' ' then c : run2 m pt1 (ty,tx) else if m ! pt2 /= ' ' then c : run2 m pt2 (-ty,-tx) else error ("no way " ++ show p)
  | 'A' <= c, c <= 'Z' = c : run2 m pd d -- 文字を踏んで先に進む
  | c == ' ' = ""
  | True = c : run2 m pd d
  where
    c = m ! p
    pd = (y+dy,x+dx)
    (ty,tx) = turn d
    pt1 = (y+ty,x+tx)
    pt2 = (y-ty,x-tx)

main2 = do
  co <- readFile "input.txt"
  let m = readMap co
  print $ length $ run2 m (findStart m) deltaD

{-
*Main> run2 testmap (findStart  testmap) deltaD
"||A||+B-+|-|+--+C||+--+D+--|E----|---F"
*Main> length it
38
*Main> main2
17420

なんか差分が簡単だったな。
-}
