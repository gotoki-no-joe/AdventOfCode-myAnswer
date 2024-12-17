# 入力

状況の地図をそのまま配列にして渡せばよいだろう。

```haskell
import Data.Array.Unboxed

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 arr = ...
```

# パート1

移動方向に関して、90度右回転した後の移動方向を求める計算と、
移動ベクトルを座標に足し込んで一歩進む計算：

```haskell
rot :: (Int,Int) -> (Int,Int)
rot (dx,dy) = (dy, - dx)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c,b+d)
```

守衛の出発地点から、範囲外に落ちるまで、忠実にシミュレーションをかける。
一度踏んだ場所に足跡をつけ、踏まれたマスを数える。

```haskell
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

part1 :: UArray (Int, Int) Char -> Int
part1 arr = runST $ do
      stain <- newArray bnds False -- 足跡
      writeArray stain pos0 True   -- 初期位置を踏む
      walking stain pos0 (-1,0)
      length . filter id <$> getElems stain
  where
    bnds = bounds arr
    -- 守衛の最初の位置
    pos0 = head [p | (p, '^') <- assocs arr]
    walking :: STUArray s (Int, Int) Bool
            -> (Int, Int) -> (Int, Int) -> ST s ()
    walking stain pos dir
      | not (inRange bnds pos1) = return () -- 踏み出すと落ちるなら終わり
      | arr ! pos1 /= '#' = writeArray stain pos1 True >> walking stain pos1 dir -- 進めるなら進む
      | otherwise = walking stain pos (rot dir) -- 進めないなら右を向く
      where
        pos1 = add pos dir
```

# パート2

障害物を追加して意味があるのは、パート1で足跡の付いた位置のみ。ただし初期位置を除く。
なので、そのような場所を記録した配列を作る計算をコードクローンで作る。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: UArray (Int, Int) Char -> Int
part2 arr = ...
  where
    bnds@(l,u) = bounds arr
    -- 守衛の最初の位置
    pos0 = head [p | (p, '^') <- assocs arr]
    -- 障害物なしで踏む場所
    strain = runSTUArray $ do
      stain <- newArray bnds False -- 足跡
      walking stain pos0 (-1,0)
      writeArray stain pos0 False   -- 初期位置を除外
      return stain
    walking ... -- 変わらず
```

どこかで落ちてしまうことは、落ちたときにわかる。
一方、どこまでも落ちずに歩き続けられることは、判定が面倒くさい。守衛の停止性問題。

- 方策1:
守衛がループせずに歩ける最大の歩数は、上下左右どちらの向きからそのマスに入るかの数で数えて、
空間のマスの個数の4倍。これを越えて、端から落ちずに歩き続けられるということは、
どこかでループに入っているはずである。  
これは、コーディングは楽だが実行時間が長そう。
- 方策2:
足跡の情報を拡張し、そのマスにどの向きから入ったかを記録する。
以前同じ方向からそのマスに入ったことがあるなら、その時点でループが発生している。  
ループが発生した瞬間を捕まえられるが、コーディングが面倒。

せっかくなので後者でやろう。

まず、向きごとに `strain` を拡張して4面にする。
進入する向きを添え字に変換する関数を立てておく。

```haskell
d2i :: (Int,Int) -> Int
d2i (-1,0) = 0
d2i ( 0,1) = 1
d2i ( 1,0) = 2
d2i (0,-1) = 3
```

拡張した `strain` を使う `walking` の拡張版を作る。
ループしたか端で落ちたかのオチだけ教えてくれればいい。

```haskell
    causeLoop arr2 = runST $ do
      strain <- newArray ((l,0),(u,3)) False
      walking2 arr2 strain pos0 (-1,0)
    walking2 :: UArray (Int,Int) Char
             -> STUArray s ((Int, Int), Int) Bool
             -> (Int, Int) -> (Int, Int) -> ST s Bool
    walking2 arr2 stain pos dir
      | not (inRange bnds pos1) = return False        -- 落ちた
      | arr2 ! pos1 /= '#' = do                       -- 進める
          b <- readArray stain (pos1, d2i dir)
          if b then return True else do               -- ループ検出
            writeArray stain (pos1, d2i dir) True
            walking2 arr2 stain pos1 dir              -- 進む
      | otherwise = walking2 arr2 stain pos (rot dir) -- 進めないなら右を向く
      where
        pos1 = add pos dir
```

ほぼコードクローンだ。

障害物を置いたマップを作成しては `causeLoop` でチェックし、ループになったものを数える。

```haskell
part2 arr = length
    [ ()
    | (obs, True) <- assocs strain
    , let arr2 = arr // [(obs, '#')]
    , causeLoop arr2 ]
  where
    ...
```

`ghci` だとちょっと時間かかるので、コンパイルして動かそう。

```haskell
main = test1 >> main1 >> test2 >> main2
```

とやっているうちにインタプリタでも結果が出ていた。
