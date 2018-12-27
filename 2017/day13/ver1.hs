import Data.List

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let ans1 = compute1 ls
  print ans1
  let ans2 = compute2a ls
  print ans2

parse :: String -> (Int,Int)
parse cs = (read ds, read rs) where
  (ds, ':':' ':rs) = break (':' ==) cs

{-
range が r であるスキャナの位置は、行き帰りを考えると
2r-2 だけの場合の数があるので、
時刻 t のときの位置は t `mod` (2r-2)

自分の位置は時刻tにdepth = tとなる。

なので、その時刻にその位置にいるスキャナを気にすればよい。
-}

testdata = [(0,3),(1,2),(4,4),(6,4)]

compute1 :: [(Int,Int)] -> Int
compute1 ls = sum [ d * r | (d,r) <- ls, d `mod` (2 * r - 2) == 0 ]

test1 = compute1 testdata

compute2 :: [(Int,Int)] -> Int
compute2 ls = head
  [ t
  | t <- [0..]
  , and [ (t+d) `mod` (2 * r - 2) /= 0 | (d,r) <- ls ]
  ]

test2 = compute2a testdata

{-
*Main> test1
24
*Main> test2
10
*Main> main
1960
3903378

結構かかった。もっとうまくやれんかな？

ようは、すべての 2r-r の倍数でない数を発見したい。
効率的なエラトステネスの篩のように、ただし長さは増えず、
という計算をしていけばいいのか？
と思ったが、tではなくてt+dなのがネックだな。
いや、+dは初期値として使えば普通にそのままいけそうだ。

そんな面倒なことしなくても、遅延計算なんだからmergeでいい。

ある遅延tの後に出発すると、自分に引っ掛かるよ、という
tの無限リストを作りたい。
そして、そのリストをmergeして出現しない整数を見つければよい。

(d,r)なスキャナは、2r-2周期で危険になる。
d=0のとき、ちょうどそれ。
d>0のとき、-dだけ早くに出てくると危険になる。
ということ。
-}

merge xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> x : merge xs yys
  EQ -> x : merge xs  ys
  GT -> y : merge xxs ys

compute2a :: [(Int,Int)] -> Int
compute2a ls = head [ i | (i,j) <- zip [0..] cs, i /= j ] where
  cs = foldr1 merge $ map conflict ls
  conflict (d,r) = dropWhile (0 >) [-d, 2*r-2-d ..]

{-
うん、ずっと早くなった。
でもなんか、最小公倍数的な何かでもっと一瞬で答えに到達できないのかな…
-}
