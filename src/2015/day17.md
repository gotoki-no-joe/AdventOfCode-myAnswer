# 17日目：多くていけないものはない

妖精はまたもや卵酒を買いすぎました、今度は150リットル。
それをすべてあなたの冷蔵庫に収めるには、小さな容器に移す必要があります。
あなたは利用可能な容器の容量のリストを書きます。

たとえば、あなたが20, 15, 10, 5, 5リットルの容器を持っているとします。
25リットルを保管する必要がある場合は、4つの方法があります。

- 15と10
- 20と5（ひとつめの5）
- 20と5（ふたつめの5）
- 15, 5, 5

使う容器はいっぱいにして使うという条件で、
150リットルの卵酒をちょうど収める
**容器の異なる組み合わせ**
はいくつありますか？

<details><summary>解説</summary><div>

入力の容器の個数は20個あるので、その総当たりの組み合わせは \\(2^20 = 1,048,576\\) と少々多い。
（力任せにやれないこともないが。）

容器を順に追加して、xxリットルを量る方法の通り数を数えるDPを行う。
最初は「0リットルを量る方法は1とおり」から、
\\(x\\) リットルの容器を追加したとき、既知の方法に \\(+x\\) した方法が足しこまれる。

```haskell
import qualified Data.IntMap as IM

part1 xs = IM.findWithDefault 0 150 im
  where
    im = foldl step (IM.singleton 0 1) xs
    step im x = IM.unionWith (+) im $ IM.mapKeysMonotonic (x +) im

main1 = readFile "input.txt" >>= print . part1 . map read . lines
```

</div></details>

# パート2

台所のあるだけの容器で遊んでいる間に、卵酒の別の配達が到着しました！
出荷部門と受入部門は、できるだけ多くの容器を使えるようにしておくことを求めています。

150リットルの卵酒をぴったり入れられる容器の最小数を見つけてください。
その数の容器をいっぱいにしてちょうど150リットルを保持できる**方法はいくつありますか？**

上記の例では、容器の最小数は2です。
その数の容器をを使用する方法は3とおりあり、
よって答は3となります。

<details><summary>解説</summary><div>

マップで記録することがらを、単なる場合の数でなく、
使う容器の個数ごとの場合の数のリストに変更する。

```haskell
part2 xs = (length as, b) -- 答えは snd、fstは使う容器の個数
  where
    im = foldl step (IM.singleton 0 [1]) xs
    step im x = IM.unionWith (zipWith1 (+)) im $ IM.mapKeysMonotonic (x +) $ IM.map (0 :) im
    (as,b:_) = span (0 ==) $ IM.findWithDefault [] 150 im

zipWith1 :: (a->a->a) -> [a] -> [a] -> [a]
zipWith1 _ xs [] = xs
zipWith1 _ [] ys = ys
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

main2 = readFile "input.txt" >>= print . part2 . map read . lines
```

</div></details>
