import Data.Char
import Data.List.Split

import Data.Array
import Control.Monad
import Data.List

import Debug.Trace

runner i f = readFile i >>= print . f. map (map read . wordsBy (not . dm)) . lines

dm '-' = True
dm c = isDigit c

test1 = runner "sample.txt" (part1 11 7)

main1 = runner "input.txt" (part1 101 103)

{-
座標を-1して 0 ～ wide - 1, 0 ～ tall -1 で考えて、
velocity*100を足してmodすれば行き先がでる。
それを4つの象限に分けてカウントする。

座標はもともと0開始だったようだ。

-}

part1 :: Int -> Int -> [[Int]] -> Int
part1 wide tall nss = product $ map (cnt !) [(LT,LT),(LT,GT),(GT,LT),(GT,GT)]
  where
    go100 [x,y,vx,vy] = (mod (x + 100 * vx) wide, mod (y + 100 * vy) tall)
    cnt = accumArray (+) 0 ((LT,LT),(GT,GT)) [((compare x100 ox, compare y100 oy),1) | r <- nss, let (x100,y100) = go100 r]
    ox = div wide 2
    oy = div tall 2

{-
たまにあるのだけど、ここまで仕様と呼べない出題は初めてでは。
全てのロボットがちょうど自分の出発点に帰ってくる時間が、調べなくてはならない全体の期間。

(x + k * vx) mod base = x
x + k * vx = x mod base
k * vx = 0 mod base
全てのvx,vyで共通にこれが成り立つkが知りたい。まずは。
それk=baseなので、つまり lcm tall wide か。

ghci> lcm 11 7
77
ghci> lcm 101 103
10403

全部見てみる？
-}

wide, tall :: Int
wide = 101
tall = 103
main = do
  nss <- map (map read . wordsBy (not . dm)) . lines <$> readFile "input.txt"
  mapM_ (printIt nss) [0 .. lcm wide tall]

printIt nss t =
  do
    print t
    forM_ [0 .. pred tall] (\y -> do
      forM_ [0 .. pred wide] (\x -> putChar $ arr ! (x,y))
      putChar '\n'
      )
  where
    arr = accumArray (flip const) '.' ((0, 0), (pred wide, pred tall))
          [(go r, '#') | r <- nss]
    go [x,y,vx,vy] = (mod (x + t * vx) wide, mod (y + t * vy) tall)

{-
105MBのテキストファイル開いて、VSCodeの縮小表示を頼りに、
なんか塊になっているのが101秒ごとにあるな、と気づいて
それを戻りながら探したら見つけられたんだけど、
実際これどうやって捕まえるのよ。
union-findで「妙にデカい集団がいる」とか？
総当たりの距離の二乗の総和が妙に小さい、とか？
座標の平均値から分散求めて、それが最小とか？これやってみるか。
めんどいので整数近似で。
-}

checkDistrib nss = take 10 $ sort [(dist t, t) | t <- [0 .. pred $ lcm wide tall]]
  where
    n = length nss
    go t [x,y,vx,vy] = (mod (x + t * vx) wide, mod (y + t * vy) tall)
    dist t = div (sum [(x - xave)^2 + (y - yave)^2 | (x,y) <- xys]) n
      where
        xys = map (go t) nss
        xave = div (sum $ map fst xys) n
        yave = div (sum $ map snd xys) n

distCheck = runner "input.txt" checkDistrib

{-
ghci> distCheck
[(764,7709),(1114,1043),(1121,4275),(1122,9123),(1142,4477),(1144,5487),(1145,3164),(1147,3063),(1147,8820),(1155,4679)]
先頭にちゃんと出てきましたね。
データサイエンスの勉強をしていたのが役に立ったかも。
-}
