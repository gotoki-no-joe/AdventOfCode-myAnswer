{-
2025/1/18 spoilerのためやりなおし

とりあえず過去の版を走らせてみる。
ver1 : modを使った力尽くの版
各時刻 t=0,1,2,…について、それぞれの円盤がそれで通れるかを試すnaiveなプログラム

ghci> phase1 example
5
(0.05 secs, 63,544 bytes)
ghci> phase1 puzzle
122318
(0.19 secs, 112,893,056 bytes)
ghci> phase2
3208583
(5.06 secs, 2,959,664,848 bytes)

それでも5secで済んでしまった。

Ver2 : modを使わないように視点を変えた版
それぞれの円盤が玉を通す出発時刻のシーケンスを作り、
二つが同時にgoサインを出す時刻だけにmergeする、で順に束ねる方法

ghci> phase1 example
5
(0.00 secs, 57,752 bytes)
ghci> phase1 puzzle
122318
(0.07 secs, 38,304,248 bytes)
ghci> phase2
3208583
(2.14 secs, 1,108,006,560 bytes)

半分になった。

Ver3 : ver2では、全てのストリームが動き続ける。
しかしもう少し考えると、二つの円盤が同時にgoになるタイミングは、両方の周期のlcmに決まっている。
ただし最初にいつ同期するかはわからないので、それはver2のやり方で求める、
という、円盤の統合のやり方を変えた方式。

ghci> compute example
5
(0.00 secs, 57,920 bytes)
ghci> compute puzzle
122318
(0.01 secs, 3,103,280 bytes)
ghci> compute $ puzzle ++ [(7,11,0)]
3208583
(0.23 secs, 108,118,232 bytes)

当時豪語しただけのことはある高速化。

----- 新しいアイデア

(1) ver2のやり方を拡張して、全てのストリームを一斉に観察する。
全ストリームの先頭の値の最大値を考える。
全てのストリームについて、それ以上の値になるまで全て捨てていい。
それまで絶対に全員一致することはないから。
dropWhileで素直にするか、divrupで飛ばして一気に求めるかは自由。

全員同じ値になったそれが答え。

(2) これChinese Remainter Theorem の定義そのままじゃん。

-}
import Control.Monad

sample, input :: [(Int,Int,Int)] -- ディスクの高さ、周期、初期位置
sample = [(1,5,4),(2,2,1)]
input = [(1,13,11),(2,5,0),(3,17,11),(4,3,0),(5,7,2),(6,19,17)]

-- (1)の方法

solve1 htps = loop $ map disc htps
  where
    disc (h, t, p) = iterate (t +) (mod (negate $ h + p) t)
    loop tss
      | all (t1max ==) t1s = t1max
      | otherwise = loop $ map (dropWhile (t1max >)) tss
      where
        t1s = map head tss
        t1max = maximum t1s

test11 = solve1 sample
main11 = solve1 input
main21 = solve1 $ (7,11,0) : input

{-
ghci> test11
5
(0.00 secs, 59,488 bytes)
ghci> main11
122318
(0.03 secs, 31,354,016 bytes)
ghci> main21
3208583
(0.76 secs, 885,347,008 bytes)

dropWhileでnaiveに捨てただけでもこの速度。
-}

-- (2) 多分これが想定解だと思うので、やっておく。

crt :: [(Int,Int)] -> Maybe (Int,Int)
crt = foldM step1 (0,1)
  where
    step1 (r0,m0) (r1,m1)
      | m0 < m1   = step2 (mod r1 m1) m1 r0 m0
      | otherwise = step2 r0 m0 (mod r1 m1) m1
    step2 r0 m0 r1 m1
      | mod m0 m1 == 0 = if mod r0 m1 == r1 then Just (r0, m0) else Nothing
      | r /= 0         = Nothing
      | otherwise      = Just (r0 + x * m0, m0 * u)
      where
        (g,im) = invGCD m0 m1
        (q, r) = divMod (r1 - r0) g
        u = div m1 g
        x = mod (mod q u * im) u

invGCD :: Int -> Int -> (Int, Int)
invGCD a b
  | a1 == 0 = (b, 0)
  | otherwise = loop b a1 0 1
  where
    a1 = mod a b
    loop s 0 m0 m1 = (s, if m0 < 0 then m0 + div b s else m0)
    loop s t m0 m1 = loop t (s - t * u) m1 (m0 - m1 * u)
      where
        u = div s t

solve2 htps = crt [(mod (negate $ h + p) t, t) | (h,t,p) <- htps]

test12 = solve2 sample
main12 = solve2 input
main22 = solve2 $ (7,11,0) : input

{-
ghci> test12
Just (5,10)
(0.01 secs, 78,656 bytes)
ghci> main12
Just (122318,440895)
(0.00 secs, 110,592 bytes)
ghci> main22
Just (3208583,4849845)
(0.00 secs, 116,000 bytes)

ノータイムでした。
-}
