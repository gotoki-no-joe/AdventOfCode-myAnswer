# 入力

`input.txt` もたった6行なので、(番号, 位置の個数(周期), 初期位置) という整数タプルのリストにする。

```haskell
sample, input :: [(Int,Int,Int)]
sample = [(1,5,4),(2,2,1)]
input = [(1,13,11),(2,5,0),(3,17,11),(4,3,0),(5,7,2),(6,19,17)]
```

# アプローチ1

玉がスタートしてからそれぞれの円盤のところまで落ちてくる時間差を考えると、
高さ \\(h\\) 周期 \\(t\\) 初期位置 \\(p\\) の円盤を玉が通過することと、
高さ \\(0\\) 周期 \\(t\\) 初期位置 \\(p + h\\) の円盤を玉が通過することは同値である。

こうして全ての円盤を高さ0に揃えると、
それぞれの円盤の隙間が位置0に来る時刻は \\(k t - (p+h)\\) である。

全ての円盤についてこの時刻を昇順の無限ストリームとして出力し、
最初に共通して現れる値を見つければよい。

このとき、

- 全てのストリームの先頭の値が全て同じか確認する
  - 同じなら、答えが見つかった
  - 異なるなら、最小値を取り除いて再帰

としたくなるが、これはちまちまやりすぎで、先頭の値の最大値に追いつくまで、次に一致する可能性はないので、

- 最大値以上になるまで全てのストリームの前方の値をそぎ落としてから再帰

とする。

```haskell
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
```

# アプローチ2

時刻 \\(x\\) が答えであるとすると、
つまり全ての円盤について \\(x = kt - (p+j)\\) を満たすような整数 \\(k\\) があるはずで、
それはつまり \\(x = -(p+j) \mod t\\) ということで、これは「中国剰余定理」そのものである。

```haskell
-- x = r mod m の連立方程式を (r, m) のリストで与える
-- 一般解 x = y mod z (z = lcm(m1,m2,...)) が存在するとき Just (y,z) を返す
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
```

多分こちらが想定解だが、アプローチ1でも問題なく解ける。
