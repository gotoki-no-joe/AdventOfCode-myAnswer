{-
センサーの位置と、「最寄り」のビーコンの位置がリストになっている。

Part 1:

ビーコンが存在しえない場所の個数を、特定のYについて数えよ、という問題。

それはつまり、いずれかのセンサーについて、そのセンサーの最寄りのビーコンよりも近いダイヤの中の空間。

センサー位置が sx,sy
最寄りのビーコンの位置が bx,by
マンハッタン距離は abs(sx-bx) + abs(sy-by) = d
より近いということで d1 = d - 1 とする、しちゃいけない。dのままで、bx,byだけ除かないといけない。うへぇ。
d1 >= 0 でないと続けない
y=Yの線に (sx,sy) から垂線を下した交点は (sx,Y) こことの距離が abs(sy-Y)
これが abs(sy-Y) <= d1 のとき、その差 d1 - abs(sy-Y) = delta だけプラスマイナスの範囲がそういう箇所
(sx - delta, Y) .. (sx + delta, Y) まで。
その集合を作って個数を数えたらいい。

-}

import qualified Data.Set as S
import Data.Char
import qualified Data.IntMap as IM
import Data.List

parse :: String -> ((Int,Int),(Int,Int))
parse xs0 = ((d1,d2),(d3,d4))
  where
    (d1,xs1) = parseSub xs0
    (d2,xs2) = parseSub xs1
    (d3,xs3) = parseSub xs2
    (d4,_  ) = parseSub xs3

parseSub :: String -> (Int, String)
parseSub = head . (reads :: ReadS Int) . tail . dropWhile ('=' /=)

dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

sensor2xs4y :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
sensor2xs4y y ((sx,sy),(bx,by))
  | delta < 0 = []
  | otherwise = [(x,y) | x <- [sx - delta .. sx + delta], x /= bx || y /= by]
  where
    d1 = dist (sx,sy) (bx,by)
    delta = d1 - abs (sy - y)

phase1 y fn = readFile fn >>= print . S.size . S.fromList . concatMap (sensor2xs4y y . parse) . lines

test1 = phase1 10 "test.txt"

main1 = phase1 2000000 "input.txt"

{-
たった24個だけど若干時間がかかった。
積分法とか使ってスキャンを一度で済ませるべきだったかね？
-}

-- phase1a y fn = readFile fn >>= print . S.fromList . concatMap (sensor2xs4y y . parse) . lines

{-
斜め45度に回転させると、チェッカーの一方の色のマスしか元のマスには対応しないけど、
マンハッタン距離が向きの揃った正方形で表現されるので、
いもす法で塗っていくのに塗りやすい。ただしどうせ、4,000,001 x 4,000,001 なんて一度にはできないから、
スキャンラインごとに確認するのだとしたら、回転なしでやっても似たようなものだと思う。
ともかく、積分法を使って、それぞれの行の関心範囲において、範囲カウントが0になる座標を洗い出す。
これは、phase1を効率的な方法でやり直すのに等しい。

のでファイルを変えよう。

変えたが戻ってきた。
全てのセンサーから距離がビーコンより離れている位置のいずれかひとつを見つけることが課題。
それらの中には、いずれかのセンサーから距離が+1であるものがある、ことを証明する。

全てのセンサーから距離が+2以上である点は答えに含まれる。
その点の4近傍は、センサーとの距離を-1するか、変えないか、+1するかのいずれか。
-1した結果はそれでも+1なので、それは答えに含まれる。

ということで、それぞれのセンサーから、距離がビーコン+1の点を列挙して、
範囲内でかつ、全てのセンサーからの距離が+1以上な点が答え。
重複するかもしらんが、同じ点になるはず。
-}

{-
part2
サンプルは (0,0),(20,20) の範囲で、
本番は (0,0),(4,000,000, 4,000,000) の範囲で、
このスキャンの範囲外となる唯一の場所を見つけよ。
ついでに x * 4,000,000 + y も計算せよ。
-}

phase2 ub fn = readFile fn >>= print . body2 ub . lines

body2 :: Int -> [String] -> [(Int,Int,Int)]
body2 ub ls =
  [ (x, y, x * 4000000 + y)
  | sxyd <- sxyds
  , (x, y) <- surrounds sxyd
  , 0 <= x, x <= ub, 0 <= y, y <= ub
  , all (ok x y) sxyds
  ]
  where
    sxyds = [(sx,sy,dist (sx,sy) (bx,by)) | ((sx,sy),(bx,by)) <- map parse ls]

surrounds (x,y,d) =
  [ p
  | let d1 = succ d, dx <- [0..d1], let dy = d1 - dx
  , p <- [(x + dx, y + dy), (x + dy, y - dx), (x - dx, y - dy), (x - dy, y + dx)]
  ]

ok x y (sx,sy,d) = dist (sx,sy) (x,y) > d

test2 = phase2 20 "test.txt"

main2 = phase2 4000000 "input.txt"

main = do
  test1
  main1
  test1a
  main1a
  test2
  main2

{-
ghciではラチがあかなかったが、-O2でコンパイルしたら
26
5181556
26
5181556
[(14,11,56000011),(14,11,56000011),(14,11,56000011),(14,11,56000011),(14,11,56000011),(14,11,56000011)]
[(3204400,3219131,12817603219131),(3204400,3219131,12817603219131),(3204400,3219131,12817603219131),(3204400,3219131,12817603219131)]
とサクサク答えがでた。しかもPart1は遅い方だこれ。
-}

-- 累積和で効率的にやる
-- ver2の定義を持ってきた

-- それぞれの行について得られた入りと出をまとめてIntMapに入れて、左から累積して、0でないマスの個数を数える
-- ただしそこから、by=y なビーコンの個数を除く必要がある！
phase1a y fn = readFile fn >>= print . body1a y . lines

body1a :: Int -> [String] -> Int
body1a y ls = loop 0 (fst $ IM.findMin m) (IM.assocs m) - cntb
  where
    rawdat = map parse ls
    m = IM.fromListWith (+) $ concatMap (sensor2xs4ya y) rawdat
    loop 0 x0 [] = 0
    loop _ _ [] = error "oops"
    loop 0 x0 ((x,d):xds) = loop d x xds
    loop h x0 ((x,d):xds) = (x - x0) + loop (h + d) x xds
    cntb = length $ nub [bx | (bx,by) <- map snd rawdat, by == y]

-- あるYについて、ある行のセンサー、ビーコンの対で塗りつぶせる範囲の入りと出
sensor2xs4ya :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
sensor2xs4ya y ((sx,sy),(bx,by))
  | delta < 0 = []
  | otherwise = [(sx - delta, 1),(sx + delta + 1, -1)]
  where
    delta = dist (sx,sy) (bx,by) - abs (sy - y)
    x1 = sx - delta
    x2 = sx + delta + 1

test1a = phase1a 10 "test.txt"
main1a = phase1a 2000000 "input.txt"
