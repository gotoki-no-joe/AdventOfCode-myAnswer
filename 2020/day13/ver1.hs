import Data.List
import Data.Function

sampleTime = 939
sampleIDs = [7,13,59,31,19]

{-
map (\id -> drop time $ cycle (1:replicate 0 (id-1))) ids
というnaiveなやり方だと、実際のデータでちょっとつらすぎる。

mod time id の結果は、時刻timeの周回がどれだけ経っているかを表す。
つまりid - (mod time id)が、次の出発までの待ち時間。
これの最小値を探せばよい。
-}

comp1 ti ids = uncurry (*) $ minimum $ map f ids
  where
    f i = (i - mod ti i, i)

test1 = comp1 sampleTime sampleIDs

myTime = 1008713
myIDs = [13,41,467,19,17,29,353,37,23]

ans1 = comp1 myTime myIDs

----------------------------

p2sample1 = [(0,7),(1,13),(4,59),(6,31),(7,19)]

{-
左がいくつめか、右がID

探している時刻tは、+aするとidの倍数になっているもの、つまり
t + a = b * n, t = bn - a
-}

p2sample2 = [(0,17),(2,13),(3,19)]
p2sample3 = [(0,67),(1,7),(2,59),(3,61)]
p2sample4 = [(0,67),(2,7),(3,59),(4,61)]
p2sample5 = [(0,67),(1,7),(3,59),(4,61)]
p2sample6 = [(0,1789),(1,37),(2,47),(3,1889)]

{-
ある二つの路線 (a1,b1) と (a2,b2) について、
t = b1m - a1, t = b2n - a2
がいえる。(b1 < b2) とおいて、b2で回してb1で判定する方が計算回数が節約できるはずだから
b2n - a2 + a1 = b1m, b2n - a2 + a1 = b1 mod m
の成り立つ左辺 LHS = b2n - a2 + a1 が見つかったとき、LHS - a1 = t である。
またこのとき td = lcm b1 b2 によって、
t + td = b1m - a1 + b1 * r1 = b1(m+r1) - a1
t + td = b2n - a2 + b2 * r2 = b2(n+r2) - a2
も同様にtとなる。
この二つの路線を一つの(a,b)で表すには、b = lcm b1 b2である。a = t か？b - t ?
-}

{-
p2step (a1,b1) (a2,b2)
  | b1 < b2 = p2stepbody (a1,b1) (a2,b2)
  | True    = p2stepbody (a2,b2) (a1,b1)

p2stepbody (a1,b1) (a2,b2) = (b - t + a1, b)
  where
    b = lcm b1 b2
    t = head $ filter ((0 ==).(flip mod b1)) $ iterate (b2 +) (b2-a2+a1)

comp2 = foldl1 p2step

test2 = comp2 p2sample1
-}

{-
なんかぜんぜん正しくない感じ。うーん。

探している時刻tは、それぞれの路線(ai,b1)に関して
t + ai = 0 mod bi
を満たすもの。一気に計算するnaiveな方法をやってみよう。
-}

{-
comp2 abs = head [ t | t <- iterate (b1 +) (- a1), null [() | (a,b) <- abs1, (t + a) `mod` b /= 0]]
  where
    (a1,b1):abs1 = sortBy (flip compare `on` snd) abs

test2 = map comp2 [p2sample1, p2sample2, p2sample3, p2sample4, p2sample5, p2sample6]

p2data = [(0,13),(3,41),(13,467),(25,19),(30,17),(42,29),(44,353),(50,37),(67,23)]

ans2 = comp2 p2data
-}

{- sample6ですでにひっかかるし、10^14以上の値にこれは無力だ。 そこから先だけ探せという意味かも知らんけど。 -}

{-
やはり...
(ai,bi)は、bi*n-aiな時刻tを意味する。
ある(a1,b1),(a2,b2)に対して、t12 = b1*m-a1 = b2*n-a2 な最小値であるとき、これは lcm b1 b2 ごとに繰り返される。
なので、次からは t12 に対して lcm を足して
-}

comp2 abs = foldl step (0,1) abs
--  where
--    (a1,b1):abs1 = sortBy (flip compare `on` snd) abs

step (t0,b0) (a,b) = (t1-a,b1)
  where
    b1 = lcm b0 b
    t1 = head $ filter ((0 ==).flip mod b) $ iterate (b0 +) (a - t0)

test2 = comp2 p2sample1
