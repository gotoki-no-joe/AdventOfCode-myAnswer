import Data.Map

{-

ちょっと前にもどこかでこんなのを考えた気がするが、すっかり忘れた。

一周回るたびに正方形が完成するが、その辺の長さはl=2n+1になっている。
つまり2ずつ伸びる。
その周を構成するマスの数は、(l-1) x4 である。(+1不要だった)
なおその最後のマスは (n,n) の位置にくる。

という分析を済ますと、順に座標を算出する方法が見えてくる。

-}

poslist = (0,0) : concat
  [ ps0 ++ ps1 ++ ps2 ++ ps3
  | n <- [0..]
  , let w = 2*n
  , let ps0 = take w $ iterate up   $ rigt (n-1, n-1)
  , let ps1 = take w $ tail $ iterate left (last ps0)
  , let ps2 = take w $ tail $ iterate down (last ps1)
  , let ps3 = take w $ tail $ iterate rigt (last ps2)
  ]

up   (a,b) = (a, pred b)
left (a,b) = (pred a, b)
down (a,b) = (a, succ b)
rigt (a,b) = (succ a, b)

ans1 = let (x,y) = poslist !! (pred 368078) in abs x + abs y

{-
後半。

Map { (0,0) -> 1 }

から始めて、poslistを辿っては周囲のマスの合計で対応を増やしていきつつ、
今書いた値をリストにして出力していく感じかな。

-}

vals = scanl func (1, singleton (0,0) 1) (tail poslist)

func (_, m) (a,b) = (s, insert (a,b) s m) where
  s = sum [ findWithDefault 0 (a+dx,b+dy) m | dx <- [-1..1], dy <- [-1..1] ]

ans2 = head $ dropWhile (<= 368078) $ fmap fst vals

{-
*Main> ans1
371
*Main> ans2
369601

ちからわざだなぁ。
-}
