# 入力

地図を二次元配列として渡す。

```haskell
import Data.Array.Unboxed

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls :: UArray POS Char
  print $ f fld

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

type POS = (Int,Int)

part1 fld = ...
```

# パート1

まずは迷路の基本的な分析をする。

```haskell
part1 fld = ...
  where
    bnds@(_,(h,w)) = bounds fld
    bfld = amap ('#' /=) fld        -- 移動可能なマスか
-- Sの位置
    sPos = head [pos | (pos, 'S') <- assocs fld]
```

4近傍の迷路を探索し、始点からの距離（到達できないときは `maxBound`）を埋めた配列を作る、距離を測る計算は幅優先探索で実装できるが、キューでなくピンポンを使うのが好き。
AoCではこれも多用している気がするので一度清書する。

```haskell
-- 4近傍迷路の距離計測
-- bnds : マスの範囲
-- max : 範囲内の位置に関して、通過できるマスか判定
-- start : 出発点
-- 出力 : 出発点からの距離、到達不能は maxBound
mazeDistance :: ((Int,Int),(Int,Int)) -> ((Int,Int) -> Bool) -> (Int,Int) -> ST s (STUArray s (Int,Int) Int)
mazeDistance bnds maze start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    loop <- fixST $ \loop -> return $ \cnt ents new -> do
      case ents of
        [] | null new  -> return ()
           | otherwise -> loop (succ cnt) new []
        ((x,y):rest) -> do
          new1 <- foldM (\new q -> do
            e <- readArray dist q
            if e < maxBound then return new else do
              writeArray dist q cnt
              return (q : new)
            ) new [q | q <- [(pred x, y), (succ x, y), (x, pred y), (x, succ y)], inRange bnds q, maze q]
          loop cnt rest new1
    loop 1 [start] []
    return dist
```

これを使って、スタートからの各マスの距離 `dist` を数える。

```haskell
-- Sから各マスまでの距離
    dist = runSTUArray $ mazeDistance bnds (bfld !) sPos
-- チートしないときの距離（確認用）
    s2e = sDist ! ePos
    ePos = head [pos | (pos, 'E') <- assocs fld]
```

動作確認として `s2e` を見てみよう。

```
ghci> test1
84
```

よさそうだ。

チートをするとは、

- 全てのコース上のマス `p` について
- そこからマンハッタン距離2のコース上のマス `q` について
- `p` から `q` へ普通に走ったときのラップタイムは `dist ! q - dist ! p`
- チートを使ったときのタイムは `2`
- この差が `gain`

そこで、`gain > 0` の値ごとに個数を数えてみる。

```haskell
    part1cnt = IM.fromListWith (+)
        [ (gain, 1)
        | (p@(x,y), True) <- assocs bfld
        , q <- [(x-2,y),(x-1,y-1),(x,y-2),(x+1,y-1),(x+2,y),(x+1,y+1),(x,y+2),(x-1,y+1)]
        , inRange bnds q, bfld ! q
        , let gain = dist ! q - dist ! p - 2
        , gain > 0]
```

見てみる。

```
ghci> test1
fromList [(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),(20,1),(36,1),(38,1),(40,1),(64,1)]
```

よさそう。  
`gain` ごとに集計する代わりに、100以上のものの個数を数えたらパート1は完成。

```haskell
    ans1 = length
        [ ()
        | (p@(x,y), True) <- assocs bfld
        , q <- [(x-2,y),(x-1,y-1),(x,y-2),(x+1,y-1),(x+2,y),(x+1,y+1),(x,y+2),(x-1,y+1)]
        , inRange bnds q, bfld ! q
        , let gain = dist ! q - dist ! p - 2
        , gain >= 100]
```

# パート2

パート1では手書きでマンハッタン距離2の位置を列挙したが、ここを、マンハッタン距離20以内の全てを列挙するものに差し替える。

ここのために `h`,`w` が必要だった。

```haskell
-- (x,y)からマンハッタン距離d以内で範囲内の座標を生成
    diamond d (x,y) =
        [ (p,q)
        | p <- [max 1 $ x - d .. min h $ x + d], let e = d - abs (x - p)
        , q <- [max 1 $ y - e .. min w $ y + e] ]
```

集計して検算する。

```haskell
    part2cnt = IM.fromListWith (+)
        [ (gain, 1)
        | (p, True) <- assocs bfld
        , q <- diamond 20 p, bfld ! q
        , let gain = dist ! q - dist ! p - mhd p q
        , gain >= 50]
```

```
ghci> test1
fromList [(50,32),(52,31),(54,29),(56,39),(58,25),(60,23),(62,20),(64,19),(66,12),(68,14),(70,12),(72,22),(74,4),(76,3)]
```

よければ解答を計算する。

```haskell
    ans2 = length
        [ ()
        | (p, True) <- assocs bfld
        , q <- diamond 20 p, bfld ! q
        , let gain = dist ! q - dist ! p - mhd p q
        , gain >=100]
```
