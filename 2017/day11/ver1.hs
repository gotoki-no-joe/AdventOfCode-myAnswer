import Data.List.Split

{-
X座標が奇数の場合、Y座標が半分上に上がっている、と解釈する。
つまり、南北はふつうに n = (0,-1), s = (0,1) だが、
東西方向は
even X のとき w = (-1, n->0, s->1) e = (1, n->0, s->-1)
odd  X のとき w = (-1, n->-1,s->0) e = (1, n->-1,s->0)
とする。

ずっと上にズラしていくやり方でもできるんだけど、
それは距離がすごく測りにくいからしない。
といっても、このやり方も「マンハッタン」距離の算出は面倒かも。
どうすればいいのかしら。

いや違うな。Y座標の小数点位置をひとつずらして、
元のままで見たとき、
Xが偶数のときはYは偶数しか使わない、
Xが奇数のときはYも奇数しか使わない、
という座標割り当てにすれば、
1ビットを贅沢に使って話が簡単になる。
-}

n  (x,y) = (x  ,y-2)
s  (x,y) = (x  ,y+2)
ne (x,y) = (x+1,y-1)
se (x,y) = (x+1,y+1)
nw (x,y) = (x-1,y-1)
sw (x,y) = (x-1,y+1)

walk "n" = n
walk "s" = s
walk "ne" = ne
walk "se" = se
walk "nw" = nw
walk "sw" = sw

{-
こうしたときマンハッタン距離は、Y軸について半分で考えればいい？
その前にX軸での距離がY軸の違いもじわじわ埋める点を計算に入れる必要がある。
-}

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = seq check $ dx + dy2 `div` 2 where
  dx = abs (x1 - x2)
  dy2 = abs (y1 - y2) - dx
  check = if even dy2 then () else error "weird dy2"

test = map (dist (0,0).($ (0,0)))
  [ne.ne.ne, sw.sw.ne.ne, s.s.ne.ne, sw.sw.se.sw.se]

main = do
  fi <- readFile "input.txt"
  let li = splitOn "," $ head $ lines fi
  let pos1 = walkover li
  print (dist (0,0) pos1)
  let ans2 = compute2 li
  print ans2

walkover :: [String] -> (Int,Int)
walkover cs = foldl (flip ($)) (0,0) $ map walk cs

{-
後半はこざかしくて泣ける。
scanl ですべての位置を出せばいいだけ。
-}

compute2 cs = maximum $ map (dist (0,0)) $ scanl (flip ($)) (0,0) $ map walk cs

{-
*Main> main
808
1556
-}
