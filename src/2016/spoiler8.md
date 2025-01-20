# パート1

命令型な内容なので、素直にそうする。

```haskell
import Data.Array.IO

part1 = do
  bm <- newArray ((0,0),(5,49)) '.' :: IO (IOUArray (Int,Int) Char)
  ls <- lines <$> readFile "input.txt"
  forM_ ls (\l -> do
    ...
```

行ごとのコマンドについて、数値が必ず2つある。これを抜き出す。

```haskell
    let [n1,n2] = map read $ wordsBy (not . isDigit) l
```

3種類のコマンドを見分ける最も簡単な方法は、`input.txt` とにらめっこすると、

```
       v
rect 1x2
rotate row y=3 by 4
rotate column x=5 by 6
       ^
```

この位置は、短い `rect` 行でも確実に文字があり、`rotate` の向きが見分けられる唯一の列である。
その文字で場合分けする。

```haskell
    case l !! 7 of
      'r' -> ...
      'c' -> ...
      _   -> ...
```

`rect` 命令は指示通り塗りつぶせばよい。

```haskell
      _   -> rect bm n1 n2

rect bm x y = forM_ (range ((0,0),(pred y, pred x))) (\p -> writeArray bm p '#')
```

`rotate` 命令は、入れ替えを行う全ての座標のリストがあれば、
その内容を読み取った列を回転させてから戻せばよい。
ここで、上から、左から考えるより逆に、下から、右からで考えると、
内容のリストを `cycle` してシフト量だけ `drop` したものが、
その位置に納まるべき内容になる。

```haskell
      'r' -> rot bm (mod n2 50) $ range ((n1,0),(n1,49))
      'c' -> rot bm (mod n2  6) $ range ((0,n1),(5,n1))

rot bm k ps = do
  let psr = reverse ps
  bs <- forM psr (readArray bm)
  zipWithM_ (writeArray bm) psr $ drop k $ cycle bs
```

せっかくなのでどんな画像が出来上がったのか表示してから、答えを数えて終わりにしよう。

```haskell
  display bm
  ans1 <- length . filter ('#' ==) <$> getElems bm
  print ans1

display bm = getElems bm >>= putStr . unlines . chunksOf 50
```

# パート2

あっ。
