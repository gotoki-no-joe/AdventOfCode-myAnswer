# 入力

整数の列が入るだけ。

```haskell
runner i f = do
  ns <- map read . lines <$> readFile i
  print $ f ns

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> Int
part1 ns = ...
```

# パート1

- 64を掛けるとは、左に6ビットシフトすること。
- 32で切り捨て除算するとは、右に5ビットシフトすること。
- 2048を掛けるとは、左に11ビットシフトすること。
- 「混ぜる」のビット単位XORをする必要がある
- Windows標準の電卓をプログラマー電卓に切り替え、16777216を打ち込んでみると、2進数で `1_0000_0000_0000_0000_0000_0000` であることがわかる。「剪定」はこれで割った余りとはつまり、下位24ビットだけを取り出す、16777215とのビット単位ANDと同じとわかる。

ということで、`Data.Bits` を使って擬似乱数生成の1ステップを定義する。

```haskell
import Data.Bits

secretStep :: Int -> Int
secretStep x = w
  where
    y = prune $ mix x $ shiftL x 6
    z = prune $ mix y $ shiftR y 5
    w = prune $ mix z $ shiftL z 11
    mix = xor
    prune x = 16777215 .&. x
```

動作確認

```
ghci> take 11 $ iterate secretStep 123
[123,15887950,16495136,527345,704524,1553684,12683156,11100544,12249484,7753432,5908254]
```

あとは、それぞれの初期値に対して2000回回した結果を足し合わせたら終わり。

```haskell
part1 :: [Int] -> Int
part1 ns = sum [iterate secretStep n !! 2000 | n <- ns]
```

# パート2

「代理の猿が次のバイヤーに進む」とか言われると、時刻とかからみそうだが、
実際には全てのバイヤーについて常に監視して、条件に合ったものがトリガーされる、
バイヤーの人数分だけ猿がいるような、シェイクスピアの猿みたいな設定。

価格が 0～9 の範囲をとるということは、ひとつの変動は -9～+9 の範囲をとる。
その4つの並びを考えるとき、例えば +9, +9 という並びはありえないが、
それを気にして、出現しうる変動4連を厳密に数え上げるのも無駄そうなので、
`((-9,-9,-9,-9),(9,9,9,9))` という配列の範囲で変動4連を網羅する。

一人一人のバイヤーについて、

- 2001要素の乱数列を作る。
- 下1桁で価格の列にする
- 変動の列を作る
- 連続する4変動とそのときの価格の組み合わせで、変動が最初に出現したものの価格を表に登録する

とすることで、初出の全ての4変動のパターンを指定したときのそのバイヤーの売値を調べ上げることができる。

```haskell
type ARR = UArray (Int,Int,Int,Int) Int

bnds :: ((Int, Int, Int, Int), (Int, Int, Int, Int))
bnds = ((-9,-9,-9,-9),(9,9,9,9))

countBuyer :: Int -> ARR
countBuyer seed = arr
  where
    ps = map (flip mod 10) $ take 2001 $ iterate secretStep seed
    ds = zipWith (-) (tail ps) ps
    d4s = zip4 ds (drop 1 ds) (drop 2 ds) (drop 3 ds)
    arr = accumArray set (-1) bnds $ zip d4s (drop 4 ps)

    set (-1) x = x
    set x    _ = x
```

-1を未定義値とみなし、`accumArray`の更新では-1が入っていたときのみ値を上書きすることで、
その変動パターンに対する最初の価格を記録しているところがポイント。

次に、全バイヤーのこの配列を要素ごとに足し合わせる。-1は無視する点に注意。

```haskell
mergeArrays :: [ARR] -> ARR
mergeArrays arrs = listArray bnds $ map (sum . map (max 0)) $ transpose $ map elems arrs
```

この配列の要素の最大値が求める値で、そのために猿に指示する列はそのような値の入っている添え字。
後者は必要ないが、せっかくなので。

```haskell
part2 ns = (ans, seq4s)
  where
    arr = mergeArrays $ map countBuyer ns
    ans = maximum $ elems arr
    seq4s = [i | (i,e) <- assocs arr, e == ans]

main2 = runner "input.txt" part2
```

動作確認

```
ghci> part2 [1,2,3,2024]
(23,[(-2,1,-1,3)])
```
