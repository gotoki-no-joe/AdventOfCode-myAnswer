-- import Data.List
import Data.List.Split
-- import qualified Data.Array as A

main = do
  fi <- readFile "input.txt"
  let ps = getpoints fi
  putStrLn "part2"
  let ans = compute ps
  print ans

getpoints :: String -> [(Int,Int)]
getpoints = map (\[at,bt] -> (read at, read bt)) . map (splitOn ", ") . lines

{-
どこまで調査する必要があるのかしら、わけわからん。
それぞれの座標から単独で10000までのダイヤ領域を
重ね合わせたところの内側にあることは確実なのだが。

いや、どれでもいいからそいつの10000以内の位置全てについてだけ
しらみつぶしに調べたらいいのか。それでも辛いけど。

もっと狭めるなら、
最も右下のものから左上に10000のライン、
云々の4つで4辺の境界を設定できる。

まぁ面倒だな。

..2..
.212.
21012
.212.
..2..

中心座標から指定された距離ぴったりの座標全てを取り出すことを考える。
はめんどいので「以内」にする。

-}

distlocs (x,y) r =
  [ (x,y)
  | i <- [0..r]
  , y <- if i==0 then [y] else [y-i,y+i]
  , x <- [x-r+i..x+r-i]
  ]

limit = 9999

compute :: [(Int,Int)] -> Int
compute ps = length
  [ ()
  | p <- distlocs (head ps) limit
  , limit >= sum (map (manh p) ps)
  ]

manh (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

{-
>ghc -O2 phase2.hs
>phase2.exe
part2
45290

コンパイルしても時間かかった。
-}
