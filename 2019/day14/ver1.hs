import Data.List.Split
import Data.List
import qualified Data.Map as M

{-
    let rs = map (fst.fst) dat
    print (length rs == length (nub rs))
結果はTrueだったので、ある物質を作る方法は一つしか存在しない。
ので、Fuelの価格は一意に定まる。
しかし、一つの変換でいくつ作られるかがものによって異なるので、
DPで一気に計算することは残念なことにできない。
-}

main = do
    co <- readFile "input.txt"
    let dat = map parse $ lines co
    let datm = M.fromList [ (n,(d,rhs)) | ((n,d),rhs) <- dat ]
    let ans = compute datm 1
    print ans
    let ans2 = compute2 datm
    print ans2
    print (compute datm ans2)

type Factor = (String,Int) -- 物質名と数
type Reaction = (Factor,[Factor])

parse :: String -> Reaction
parse li = (parse1 rhs, map parse1 lhss)
  where
    [lhs,rhs] = splitOn " => " li
    lhss = splitOn ", " lhs

parse1 :: String -> Factor
parse1 dn = (n, read d)
  where
    [d,n] = words dn

{-
必要量表：初期値 [("FUEL",1),他0]
から始めて、
(1) "ORE"でなく、量が正のものをいずれか選択。その必要量をXとする
(2) 反応表から、一度の変換で作れる量Yを得る
(3) (X+Y-1) `div` Y = Z 倍、反応を行う。原料が反応表のZ倍必要。
(4) Y*Z - X = X `mod` Y = A だけ作りすぎる。
(5) 必要量表に、原料をそれぞれZ倍足す、選択した物質を Y*Z減らす（マイナスは余剰を表す）、をする
(6) 必要量表が、"ORE"以外全て0以下になっていれば完了、さもなくば(1)から繰り返し

(6)の終了条件と、(1)の選択するものがあることが双対
-}

-- M.Map String (Int,[Factor])

compute :: M.Map String (Int, [Factor]) -> Int -> Int
compute datm vol = loop init
  where
    init = M.singleton "FUEL" vol
    loop req = case select req of
        Nothing -> req M.! "ORE"
        Just (item,x) ->
          let
            (y,rhs) = datm M.! item
            z = (x+y-1) `div` y
            req1 = M.unionWith (+) req $ M.fromList $ (item, -y*z) : [ (i,q*z) | (i,q) <- rhs]
          in
            loop req1

-- 反応させるものを選択
select :: M.Map String Int -> Maybe (String, Int)
select req = if null kas then Nothing else Just (head kas)
  where
    kas = [ ka | ka@(k,a) <- M.assocs req, k /= "ORE", a > 0 ]

-- part2
-- 雑な二分探索でpart1を使って、在庫を超えずに作れるFUELの最大量を特定する
-- 1から2倍を続けて足りなくなる一つ前を決めて、
-- そこから、もう半分を足してよいかどうかを1まで調べる。

trillion = 1000000000000

compute2 :: M.Map String (Int,[Factor]) -> Int
compute2 datm = loop (div i 2) (div i 4)
  where
    i = head $ filter ((trillion <=).compute datm) $ iterate (2*) 1
    loop acc 0 = acc
    loop acc i = let acc' = acc + i in loop (if compute datm acc' <= trillion then acc' else acc) (i `div` 2)

{-
*Main> main
301997
6216589
999999955130
-}
