# パート1

## 素朴な解法1

プレゼントの内容は無視して、ゲームに残っている人だけ追跡する。
注目している位置の二人のうち、盗む方は残り、盗まれた方は次に自分の番が回ってきて抜けることになる。

小人はN人とする。
命令型言語なら、配列を使うなりして大きさNのリングバッファに1からNを書き込み、
1から順に、2つ消しては前の値を末尾に追加する、を最後の一つになるまでN-1ステップ行うことで
答えに到達できる。

`Data.Sequence` を使えば、前から要素を取り去りつつ後ろに追加するという両側への操作を
効率的に実装できる。

```haskell
import qualified Data.Sequence as Q

sample, input :: Int
sample = 5
input = 3014387

part1a n = loop $ Q.fromList [1 .. n]
  where
    loop (a Q.:<| _ Q.:<| q) = loop (q Q.:|> a)
    loop (a Q.:<| Q.Empty) = a
```

```
ここに実行結果
```

## 素朴な解法2

ループ状の構造は、遅延参照を使ったリストでも表現できる。
ただし、現在の要素数を別で管理していないと、要素がなくなった瞬間がわからなくて困る。
1ステップ処理するたびに一人抜けるので、残り人数が1になったら止めればよい。

```haskell
part1b n = last loop
  where
    loop = iter n ([1 .. n] ++ loop)
    iter 1 (a:_) = [a]
    iter k (a:_:as) = a : iter (pred k) as
```

```
ここに実行結果
```

以上は \\(O(N)\\) である。

## 解析的な解法

背番号を0始まりで振って考える。

すると普通のゲームの一周は、背番号の最下位桁が0の人が残り、1の人が抜ける。
ここで(A)最後の人が抜けるとき、次の周も「普通のゲーム」になる。
残った人たちでゲームを再開するが、これは、半分の人数で、
背番号を1ビット右シフトした人たちがゲームを開始するのと同じ状況になるので、再帰的に答えが求められる。

再帰の終わりは、人数が1人、つまり最大の背番号が0のときである。

(A)でない場合、最後の人が抜けずに残る場合を考える。
するとその人の番になることで、次の周では背番号の最下位桁が0の人が抜けるという、逆のゲームが起きる。
ゲームが一巡したとき、末尾の人が残る側のとき逆のゲーム、抜けるとき普通のゲームが次の周回になる。

末尾の人が抜けるとき、そのもう一人前の人に末尾がずれる。
このとき、背番号の最下位桁が1なら、2で割った背番号は末尾の人とその前の人で変わらない。
最下位桁が0のとき、2で割った背番号をさらに1減らす必要がある。

以上のロジックをコードにする。

```haskell
part1c n = succ $ loop 0 (pred n)
  where
    loop _ 0 = 0 -- 末尾の番号が0なら、唯一なので終わり
    loop b k -- 今回の桁はb
      | b == r    = b + 2 * loop 1 q
      | otherwise = b + 2 * loop 0 (q - b) -- r=0(b=1)ならqを減らす
      where
        (q,r) = divMod k 2 -- 末尾の最下位ビットとそれ以外
```

これは \\(O(\log N)\\) 二進数の桁数ぶんの計算量で答えが得られる。

## 観察

算出された答えを二進数で観察すると、
「人数のMSBを0にし、左1ビットシフトした結果に1を足した値」
がこの問題の答えのように見える。
様々な値で試しても確かにそうなる。これはどういうことだろう？

```haskell
msb :: Int -> Int -- 64bit only
msb x = loop 32 x
  where
    loop _ 1 =  1
    loop b x
      | x1 == 0   = loop (b .>>. 1) x
      | otherwise = loop (b .>>. 1) x1 .<<. b
      where
        x1 = x .>>. b

part1d n = succ $ (msb n .^. n) .<<. 1
```

MSBの求め方にもよるが、上のコードで \\(O(\log \log N)\\) で終わる。

# パート2

人数が奇数のときは、前側から取れといっている。

取る側前半と、取られる側の先頭からの後半とに、列を二分して考える。

全体の人数が偶数 \\(2m\\) のとき
```
1 2 ... m | m+1 ... m+m        m:m
o           xxx                取る側の先頭が取られる側の先頭を取る。1は一番後ろに並び直す
  2 ... m |    m+2 .. m+m 1    m-1 : m
                               人数差は1、取る側の方が少ないので、m+2は次に取られる先頭のままでよい
```
全体の人数が奇数 \\(2m+1\\) のとき、前半の方を少なく分ける
```
1 2 ... m | m+1 ... 2m 2m+1    m:m+1
o           xxx                m+1が取られる。1は末尾へ
  2 ... m |    m+2 .. 2m+1 1   m-1 : m+1
                               人数差が2は大きすぎる。バランスのためm+2は前半に移動
  2 ... m m+2 |  m+3 ...   1   m : m
                               バランスした。次はm+3が取られる
```

この様子をパート1同様にシミュレーションできる。

```haskell
part2a n = loop (even n) (Q.fromList [1 .. m]) (Q.fromList [succ m .. n])
  where
    m = div n 2
    loop False Q.Empty    (a Q.:<| Q.Empty) = a
    loop True  (a Q.:<| q) (_ Q.:<| r)         = loop False q          (r Q.:|> a)
    loop False (a Q.:<| q) (_ Q.:<| b Q.:<| r) = loop True (q Q.:|> b) (r Q.:|> a)
```

```
ghci> part2a sample
2
(0.01 secs, 63,640 bytes)
ghci> part2a input
1420064
(5.55 secs, 3,479,625,400 bytes)
```

二つのキューを使う代わりに、取られる側の先頭から始まる単一のキューを考える。

- 全体が偶数個のとき、先頭が取られて、全体は奇数個になる。
- 全体が奇数個のとき、先頭が取られて、バランスのために先頭は末尾に移動する。

```haskell
part2b n = loop (even n) $ Q.fromList $ [succ m .. n] ++ [1 .. m]
  where
    m = div n 2
    loop False (a Q.:<| Q.Empty) = a
    loop True  (_ Q.:<| q) = loop False q
    loop False (_ Q.:<| b Q.:<| q) = loop True (q Q.:|> b)
```

```
ghci> part2b sample
2
(0.00 secs, 62,912 bytes)
ghci> part2b input
1420064
(2.15 secs, 2,032,759,064 bytes)
```

キューが一つなら、`part1b` 同様にリストでも実装できる。

```haskell
part2c n = last loop
  where
    m = div n 2
    loop = iter n ([m+1..n] ++ [1..m] ++ loop)
    iter 1 (x:_) = [x]
    iter n (_:xs)
      | even n = iter (pred n) xs
    iter n (_:y:xs)
               = y : iter (pred n) xs
```

```
ghci> part2c sample
2
(0.01 secs, 64,176 bytes)
ghci> part2c input
1420064
(3.86 secs, 2,387,466,528 bytes)
```

シミュレーションせずに解く方法があるのかよくわからない。
