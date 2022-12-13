<!-- 五時之丞 いいなそれ。 如之丈 なんだが。-->

# 12 日目: 山登りアルゴリズム

あなたは携帯機器を使用して小人と連絡を取ろうとしましたが、
あなたが辿っている川は位置が低すぎて適切な信号を受信できませんでした。

機器に周辺領域の高さ地図を要求します（パズル入力）。
高さ地図は、グリッドに分割された上空から見た周辺地域を示しています。
グリッドの各正方形の標高は、1 つの小文字で示されます。
ここで `a` は最低の標高、`b` は次に低い標高、というように最高の標高 `z` まで続きます。

高さ地図には他に、現在の位置 (`S`) と最良の信号を受信できる位置 (`E`) の印も含まれています。
現在の位置 (`S`) は標高 `a` を持ち、
最高の信号を受信できるはずの場所 (`E`) は標高 `z` を持ちます。

あなたは `E` に到達したいのですが、エネルギーを節約するために、
できるだけ**少ないステップ数で**到達する必要があります。
各ステップでは、ちょうど1マス上下左右に移動できます。
登山用具を取り出さなくても済むように、目的地のマスの標高は、
現在のマスの標高より**たかだか1だけ高くなる**ならば許されます。
つまり、現在の標高が `m` の場合、標高 `n` に進むことはできますが、標高 `o` に進むことはできません。
（これは、目的のマスの標高が現在のマスの標高よりもずっと低くなる方は構わないことも意味しています。）

例えば：

```
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
```

ここでは、左上隅から始めます。あなたの目的地は真ん中近くです。
下または右に移動することから始めることもできますが、
最終的には一番下の `e` に向かう必要があります。
そこから、螺旋状に進むことで目的地へ行くことができます。

```
v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^
```

上の図における記号は、上 (`^`)、下 (`v`)、左 (`<`)、右 (`>`)に移動してそのマスから出る経路であることを示します。
最良の信号を受信できるはずの場所はここでも `E` で示され、`.` は訪問されないマスの印です。

この経路は、31ステップで目的地に到達します。これは可能な限り少ない数です。

**現在の位置から最適な信号を受信できる場所に移動するのに必要な最小のステップ数はいくつですか？**

<!--
<details><summary>解説</summary><div>

幅優先探索で、出発地点から到達できたマスに到達済みの印を書き込んでいく。
目的地のマスが書き込まれたところで繰り返しを終了する。
これをするためには、到達済みフラグの、書き換え可能な二次元配列が必要。
さらに、標高地図もランダムアクセスしたいので二次元配列に保存する。

地図は行ごとの文字列のリストとして読み込む。

```haskell
phase1 fn = do
  ls <- lines <$> readFile fn
  ...
```

ここから、`S`の位置と`E`の位置を見つけておく。

```haskell
phase1 fn = do
  ...
  let [spos] = locate 'S' ls
  let [epos] = locate 'E' ls
  ...

locate v xss = [(i,j) | (i,xs) <- zip [1..] xss, (j,x) <- zip [1..] xs, v == x]
```

標高地図の二次元配列を作る際は、`S`と`E`はその標高に置き換える。

```haskell
import Data.Array.Unboxed

phase1 fn = do
  ...
  let w = length (head ls)
  let h = length ls
  let a2z = listArray ((1,1),(h,w)) $ map se2az $ concat ls :: UArray (Int,Int) Char
  ...

se2az 'S' = 'a'
se2az 'E' = 'z'
se2az  c  =  c
```

フラグの二次元配列の初期値は到達済みを `True` とする。

```haskell
import Data.Array.IO

phase1 fn = do
  ...
  da <- newArray ((1,1),(h,w)) False :: IO (IOUArray (Int,Int) Bool)
  ...
```

これらを利用して、幅優先探索を行う。
未処理の地点のリストをひとつずつ消費し、そこが到達済みなら捨てる。
未到達なら、到達済みとチェックし、その周辺4地点の標高が+1までに収まっていれば、
次に調査するべき地点のリストに加える。
処理する地点のリストが尽きたら、
距離を1増やして、次に調査するべき地点のリストを処理する地点のリストに持ち替えて繰り返す。
目的地の距離が判明した時点で終了する。

```haskell
import Data.Ix

phase1 fn = do
  ...
  loopWrap a2z da epos spos >>= print

loopWrap :: UArray (Int, Int) Char -> IOUArray (Int,Int) Bool -> (Int,Int) -> (Int,Int) -> IO Int
loopWrap a2z da epos spos = loop 0 [spos] []
  where
    bnds = bounds a2z
    loop :: Int -> [(Int,Int)] -> [(Int,Int)] -> IO Int
    loop _ [] [] = error "cannot reach to E"
    loop cnt [] qs = loop (succ cnt) qs []
    loop cnt (p:ps) qs =
      do
        b <- readArray da p
        if b then loop cnt ps qs else do
          writeArray da p True
          if p == epos then return cnt else do
            let h1 = succ $ a2z ! p
            qs1 <- foldM (\qs d -> do
              let q = add p d
              if not (inRange bnds q) || h1 < a2z ! q then return qs else do
                d <- readArray da q
                return $ if d then qs else q:qs
              ) qs deltas
            loop cnt ps qs1
```

</div></details>
-->

# パート2

丘を上っていくとき、小人がここをハイキングコースにしようとしているのではないかとあなたは訝しみました。
ただし、この出発地点はあまり風光明媚ではありません。
おそらく、より良い出発点を見つけることができるでしょう。

ハイキング中の運動を最大限にするために、コースはできるだけ低い標高、つまり `a` から始める必要があります。
目的地はやはり`E` と印されたマスです。
ただし、コースはやはり最も近道である必要があります。
つまり、目的地に到達するためのステップ数は最小で済ませてください。
したがって、標高 `a` である任意のマスから、`E` と印されたマスまでの最短経路を見つける必要があります。

上記の例をもう一度考えてみましょう。

```
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
```

ここで、開始位置には6つの選択肢があります。
（`a` と印された5つと、高度 `a` にあると見なされる `S` と印されたマス）
左下のマスから始めると、最も早くゴールに到達できます。

```
...v<<<<
...vv<<^
...v>E^^
.>v>>>^^
>^>>>>>^
```

この経路は、可能な限り少ない手順である**29**ステップで目的地に到達します。

**標高 `a` の任意のマスから出発して、最良の信号を受信できる場所に移動するのに必要な最小ステップ数はいくつですか？**

<!--
<details><summary>解説</summary><div>

幅優先探索の動作を少し修正する。

- `E`の地点から出発する。
- 移動できる地点の標高の判断が逆になる。
- 新たに到達した地点の標高が初めて `a` であったとき、その距離が答えである。

```haskell
phase2 fn = do
  ...
  loop2Wrap a2z da epos >>= print

loop2Wrap :: UArray (Int, Int) Char -> IOUArray (Int,Int) Bool -> (Int,Int) -> IO Int
loop2Wrap a2z da epos = loop 0 [epos] []
  where
    bnds = bounds a2z
    loop :: Int -> [(Int,Int)] -> [(Int,Int)] -> IO Int
    loop _ [] [] = error "cannot reach to height 'a'"
    loop cnt [] qs = loop (succ cnt) qs []
    loop cnt (p:ps) qs =
      do
        b <- readArray da p
        if b then loop cnt ps qs else do
          writeArray da p True
          if a2z ! p == 'a' then return cnt else do
            let h1 = pred $ a2z ! p
            qs1 <- foldM (\qs d -> do
              let q = add p d
              if not (inRange bnds q) || h1 > a2z ! q then return qs else do
                d <- readArray da q
                return $ if d then qs else q:qs
              ) qs deltas
            loop cnt ps qs1
```

</div></details>
-->
