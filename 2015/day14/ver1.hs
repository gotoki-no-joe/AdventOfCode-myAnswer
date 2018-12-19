import Data.List (transpose, sort)

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let ans1 = compute1 ls
  print ans1
  putStrLn "part2"
  let ans2 = compute2 ls
  print ans2

type Line = (String,Int,Int,Int)

parse :: String -> Line
parse cs = (ws !! 0, get 3, get 6, get 13) where
  ws = words cs
  get k = read $ ws !! k

{-
xx は v km/s で a sec 飛んで、b sec 休む。
とは、a sec だけ v の傾き、b sec だけフラットな、斜め階段状の距離グラフを描く。

t秒後での位置は、(q,r) = t `divMod` (a+b) として、
q回はa*vだけ飛びきっているのでq*a*v
さらに (r `min` a) * v だけ追加で飛んでいる。
と導けた。
-}

dist v a b t = (q*a + (r `min` a)) * v where
  (q,r) = t `divMod` (a+b)

compute1 :: [Line] -> [(Int,String)]
compute1 ls = filter ((maxd ==).fst) ds where
  ds = [ (dist v a b 2503, x) | (x,v,a,b) <- ls ]
  maxd = maximum $ map fst ds

{-
後半戦をみると、シミュレーションをちゃんとやってもよかったんだなという。
しかしHaskellらしさに拘ってみようか。

xx は v km/s で a sec 飛んで、 b sec 休む。
とき、それぞれの時刻で進む距離のリストは

cycle (replicate a v ++ replicate b 0)

で作れる。
毎秒の到達距離は、足し合わせて

scanl1 (+) ...

で作れる。
それぞれの位置のリストは transpose で切り出せる。

最大値であった位置に1、それ以外に0とポイントに読み替える計算は別に書く。
それをレースの距離だけ切り出して、総和をとる。
-}

compute2 :: [Line] -> [(Int,String)]
compute2 ls = sort $ zip (map sum $ transpose $ take 2504 $ map rank2point $ transpose [ scanl1 (+) $ cycle (replicate a v ++ replicate b 0) | (x,v,a,b) <- ls ]) [ x | (x,_,_,_) <- ls ]

rank2point :: Ord a => [a] -> [Int]
rank2point xs = [ if top == x then 1 else 0 | x <- xs ] where
  top = maximum xs

{-
*Main> main
[(2655,"Donner")]
part2
[(0,"Dasher"),(1,"Dancer"),(5,"Blitzen"),(13,"Cupid"),(22,"Comet"),(153,"Prancer"),(415,"Donner"),(887,"Rudolph"),(1059,"Vixen")]
-}
