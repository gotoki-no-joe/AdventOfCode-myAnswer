# 20日目：無限の妖精と無限の家

妖精たちを忙しくしておくために、サンタは彼らにいくつかのプレゼントを
ドアからドアへ、手渡しで配達させます。
サンタは妖精たちを
順に番号が1,2,3,4,5…と振られている家が無限に並ぶ通りに
送り込みました。

各妖精にも番号が割り当てられており、
その番号に基づいて家にプレゼントを配達します。

- 1番目の妖精（1番）はすべての家にプレゼントを配達します。1, 2, 3, 4, 5, …
- 2番目の妖精（2番）はひとつおきの家にプレゼントを配達します。2, 4, 6, 8, 10, …
- 3番目の妖精はふたつおきのすべての家にプレゼントを配達します。3, 6, 9, 12, 15, …

1で始まる番号が付けられた無数の妖精がいます。
各妖精は家ごとに自分の数の10倍に相当するプレゼントを配達します。

よって、通りの最初の9つの家は最終的に次のようになります。

~~~
House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.
~~~

1番目の家はプレゼント10個受け取ります。
それはプレゼントを \\(1 \times 10\\) 個届ける1番の妖精によってのみ訪問されます。
4番目の家は番号1,2,4の妖精が訪れているので、
合計で \\(10 + 20 + 40 = 70\\) 個のプレゼントを受け取ります。

あなたのパズル入力の数と少なくとも同じだけの数のプレゼントを得る家の、
**最も小さい家番号**は何番ですか？

あなたのパズル入力は36000000です。

<details><summary>解説</summary><div>

家番号 \\(N\\) が \\(a\\) の倍数であるとき、プレゼントを \\(10a\\) 受け取る、
その総和が目標を超えるような最小の \\(N\\) を見つけたい。
家番号 \\(N\\) に、妖精 \\(N\\) は必ず訪問するので、 \\(N \leq 3,600,000\\) が上限。

```haskell
import Data.Array.Unboxed

theInput :: Int
theInput = 36000000
theInput10 = div theInput 10

part1 = head $ filter ((theInput10 <=) . snd) $ assocs arr
  where
    arr :: UArray Int Int
    arr = accumArray (+) 1 (1,theInput10)
          [(j, i) | i <- [2 .. theInput10], j <- [i, i+i .. theInput10]]
```

探索範囲を倍々に増やしながら答えを見つけるまで続けるようなコードにすることもできる。

```haskell
part1a = loop 1 100
  where
    loop lb ub
      | null ans = loop (succ ub) (min theInput (ub * 2))
      | otherwise = head ans
      where
        arr :: UArray Int Int
        arr = accumArray (+) 1 (lb,ub)
          [(j, i) | i <- [2..ub], let j0 = i * divrup lb i, j <- [j0, j0+i .. ub]]
        ans = filter ((theInput10 <=) . snd) $ assocs arr

-- 切り上げ除算
divrup x y = negate $ div (negate x) y
```

こちらの方が少し速く完了する。

</div></details>

# パート2

妖精たちは無限の数の家を訪れたくないと考えました。
代わりに、妖精はそれぞれ50軒にプレゼントを配達した後に停止します。
それを補うために、
彼らは各家で自分の番号の11倍に相当するプレゼントを届けることにしました。

これらの変更により、
あなたのパズル入力の数と少なくとも同じ数のプレゼントを得る家の
新しい**最小の家番号**は何番ですか？

<details><summary>解説</summary><div>

家番号 \\(K\\) に訪れる妖精の番号は \\(K\\) 以下である。
\\(K=1\\) から順に、妖精 \\(K\\) の配達まで済ませた状況を作り、家 \\(K\\) の個数を確認する。
目標に達していなければさらに次に進む。このとき、\\(K\\) 以下の家の状況は今後更新されないので捨ててよい。

```haskell
import qualified Data.IntMap as IM

part2 = loop 1 IM.empty
  where
    loop k im
      | n >= theInput = (k, n)
      | otherwise     = loop (succ k) im1
      where
        ((k1, n), im1) =
          IM.deleteFindMin $
          IM.unionWith (+) im $
          IM.fromDistinctAscList $
          [(i, 11 * k) | i <- map (k *) [1 .. 50]]
```

</div></details>
