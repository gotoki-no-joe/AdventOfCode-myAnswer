import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Array

{-
交互に並ぶ配置
 0 1 2 3 4 5
0 1 2 3 4 5
 0 1 2 3 4 5
0 1 2 3 4 5
だと、n,sの移動が0,+1か-1,0かが奇遇で変わる
ずっとずれ続ける配置
   0 1 2 3 4 5
  0 1 2 3 4 5
 0 1 2 3 4 5
0 1 2 3 4 5
だと、気にせずnは-1,0、sは0,+1で固定できる。
part1を解くだけなら、これでできるからそうしよう。

w : x-
e : x+
nw,ne : y-, x-/x0
sw,se : y+, x0/x+
-}

line2xy :: String -> (Int,Int)
line2xy = loop 0 0
  where
    loop x y ('w':str) = loop (pred x) y str
    loop x y ('e':str) = loop (succ x) y str
    loop x y ('n':'w':str) = loop (pred x) (pred y) str
    loop x y ('n':'e':str) = loop x        (pred y) str
    loop x y ('s':'w':str) = loop x        (succ y) str
    loop x y ('s':'e':str) = loop (succ x) (succ y) str
    loop x y "" = (x,y)

compute1 :: [(Int,Int)] -> S.Set (Int,Int)
compute1 = foldl' step S.empty
  where
    step s xy
      | S.member xy s = S.delete xy s
      | True = S.insert xy s

part1 fn = readFile fn >>= pure . compute1 . map line2xy . lines

test1 = part1 "sample.txt" >>= print . S.size

ans1 = part1 "input.txt" >>= print . S.size

{-
*Main> test1
10
*Main> ans1
424

あれ、後半も多分大したことないねこれ。
前のようにリストのzipでできると楽なのだけど、
黒をTrueで表す[[Bool]]を基本にして、これを[[Int]]にした後、
そのマスの周囲の黒の個数な[[Int]]をzip (+)で作って、
次の状態を計算する感じ。
-}

{-
Set (Int,Int) から、xyの最大最小を求め、初期配置リストを作る、大きさはぎりぎりでいい。
-}

{-
buildMap :: S.Set (Int,Int) -> [[Bool]]
buildMap s = [[S.member (x,y) s | x <- [xl..xu]] | y <- [yl..yu]]
  where
    (xs,ys) = unzip $ S.elems s
    (xl,xu,yl,yu) = (minimum xs, maximum xs, minimum ys, maximum ys)
-}

{-
1ステップ進める
-}

{-
generation :: [[Bool]] -> [[Bool]]
generation bss =
  where
    nss1 = map (map (\b -> if b then 1 else 0)) bss
    w = length (head bss)
    nss2 = map (\ns -> head ns : zipWith (+) ns (tail ns) ++ [last ns]) nss1
    zeros = iterate (w+2) 0
    nss3 = zeros : nss2 ++ [zeros]

毎ターン周囲が１コマずつ拡張されるのと、
端のマスのカウントを落とさず数えるための仮想0拡張が
同時に起きて混乱する。わけわからん。
-}

{-
クールにリストでするのが面倒になったので、配列でごまかす。
-}

type Stage = Array (Int,Int) Bool

buildArr :: S.Set (Int,Int) -> Stage
buildArr s = accumArray (||) False ((xl,yl),(xu,yu)) [(p,True) | p <- S.elems s]
  where
    (xs,ys) = unzip $ S.elems s
    (xl,xu,yl,yu) = (minimum xs, maximum xs, minimum ys, maximum ys)

generation :: Stage -> Stage
generation ba = array ((lx1, ly1),(ux1, uy1)) al
  where
    ((lx,ly),(ux,uy)) = bounds ba
    (lx1,ly1,ux1,uy1) = (pred lx, pred ly, succ ux, succ uy)
    access x y
      | lx <= x && x <= ux && ly <= y && y <= uy && ba ! (x,y) = 1
      | True = 0
    al = [((x,y),f x y) | x <- [lx1..ux1], y <- [ly1..uy1]]
    f x y = s == 2 || s == 1 && access x y == 1
--      | access x y == 1 = s == 1 || s == 2
--      | True = s == 2
      where
        (x0,x1,y0,y1) = (pred x,succ x,pred y,succ y)
        s = access x0 y0 + access x y0 + access x0 y + access x1 y + access x y1 + access x1 y1

countArr a = length $ filter id $ elems a

test2 = part1 "sample.txt" >>= pure . map countArr . take 101 . iterate generation . buildArr

ans2 = part1 "input.txt" >>= pure . map countArr . take 101 . iterate generation . buildArr

{-

*Main> test2
[10,15,12,25,14,23,28,41,37,49,37,55,54,69,73,84,92,88,107,113,132,133,147,134,177,170,176,221,208,207,259,277,283,270,324,326,333,345,371,380,406,439,466,449,478,529,525,570,588,576,566,636,601,667,672,735,766,723,755,805,788,844,875,908,936,994,943,1015,1029,1058,1106,1158,1146,1125,1159,1202,1344,1277,1345,1320,1373,1420,1431,1469,1561,1590,1596,1699,1662,1788,1844,1797,1800,1866,1887,1878,2070,1930,2031,2088,2208]
*Main> :r
[1 of 1] Compiling Main             ( ver1.hs, interpreted )
Ok, one module loaded.
*Main> ans2
[424,190,243,276,331,323,386,402,403,457,477,493,536,454,526,559,567,604,593,622,670,722,725,758,772,804,849,807,901,865,892,943,943,1016,1026,1028,1076,1118,1133,1241,1168,1158,1291,1254,1305,1346,1394,1416,1505,1455,1538,1558,1553,1590,1613,1685,1704,1820,1819,1744,1900,1845,1964,1943,2079,2085,2130,2188,2173,2281,2210,2361,2444,2441,2432,2673,2543,2725,2681,2758,2774,2898,2928,2960,2952,3018,3143,3109,3176,3238,3315,3286,3388,3478,3562,3593,3721,3634,3626,3752,3737]

くっそ重いのは(Array.!)がO(1)でなくてO(log n)なせいなのか？
広げる必要のない範囲まで配列広げてるからか？Vector使えば解決するこれ？

この程度の点の数なら、Set (Int,Int) のままにして、Map (Int,Int) Int に個数を数えて、==2またはmemberかつ==1というやり方の方が速かったかもだ。
-}

ds = [(-1,-1),(0,-1),(-1,0),(1,0),(0,1),(1,1)]

geneSet :: S.Set (Int,Int) -> S.Set (Int,Int)
geneSet s = S.fromList [(x,y) | ((x,y),c) <- M.assocs m, c == 2 || S.member (x,y) s && c == 1]
  where
    m = M.fromListWith (+) [((x+dx,y+dy),1) | (x,y) <- S.elems s, (dx,dy) <- ds]

test3 = part1 "sample.txt" >>= pure . map S.size . take 101 . iterate geneSet

ans21 = part1 "input.txt" >>= pure . map S.size . take 101 . iterate geneSet

-- はい、ずっと早く終わりました。てへ。まだ遅いけど。

{-
-- リストもやっぱり作って比べてみるか

buildMap :: S.Set (Int,Int) -> [[Bool]]
buildMap s = [[S.member (x,y) s | x <- [xl..xu]] | y <- [yl..yu]]
  where
    (xs,ys) = unzip $ S.elems s
    (xl,xu,yl,yu) = (minimum xs, maximum xs, minimum ys, maximum ys)

-- geneList :: [[Bool]] -> [[Bool]]
geneList bss = {- zipWith (zipWith f) nss1trim-} nss_2
  where
    w = length (head bss)
    zeros = replicate (w+2) 0
    -- nss1 : bssのFalse/Trueを0/1に置き換えて、2回り広げたもの
    nss1 = zeros : zeros : map (\bs -> 0:0:map (\b -> if b then 1 else 0) bs ++[0,0]) bss ++ [zeros,zeros]
    -- nss_ns : 隣接要素を足し合わせた、北と南の計算のための中間データ
    nss_ns = map (\ns -> zipWith (+) ns (tail ns)) nss1
    -- nss_ew : 一つ飛びに足し合わせた、西と東を足した中間データ
    nss_ew = map (\ns -> zipWith (+) ns (drop 2 ns)) nss1
    -- nss_2 : 北、東西、南を足し合わせた、周囲の黒の個数
    nss_2 = zipWith3 (\ns_n ns_ew ns_s -> zipWith3 add3 (0 : ns_n) ns_ew ns_s) nss_ns (tail nss_ew) (drop 2 nss_ns)
    add3 a b c = a + b + c
    nss1trim = map (init . tail) $ init $ tail $ nss1
    f _ 2 = True
    f 1 1 = True
    f _ _ = False

-- test4 = part1 "sample.txt" >>= pure . map (length . filter id . concat) . take 101 . iterate geneList . buildMap

速いけどズレが直らないので放棄。

-}
