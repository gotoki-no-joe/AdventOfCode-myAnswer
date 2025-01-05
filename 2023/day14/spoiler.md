# 入力

行ごとに分けるくらいしかやれることがない。

```haskell
runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = ...
```

# パート1

変更後のマップ全体を mutable array で作るのは考えるだけで滅入る。
`transpose` して列ごとに考え、前から調べて

- `#` のとき：次に進む
- `O` のとき：次に進む。現在の南端からの行数だけ荷重がかかる。
- `.` のとき：次の `#` または行末までに `O` があるなら、それをここに移動させる。荷重がかかる。次に進む。
移動させられる`O`がない場合、次の`#`まで進めるか、行末まで何もなければ終了する

<!-- とできる。効率を気にしなければ、`.` で空振りしたときも一歩ずつ進めるでもよい。-->

```haskell
import Data.List

part1 ls = sum $ map (score h) $ transpose ls
  where
    h = length ls
    score _ "" = 0
    score s ('#':xs) = score (pred s) xs
    score s ('O':xs) = s + score (pred s) xs
    score s ('.':xs) =
      case (elemIndex 'O' xs, elemIndex '#' xs) of
        (Nothing, _)             -> 0                                -- 動く岩はひとつもない
        (Just p, Just q) | q < p -> score (pred s - q) $ drop q xs   -- 動く岩は次の#より向こうにしかない
        (Just p, _)              -> s + score (pred s) (remAt p xs)  -- 動く

remAt p xs = take p xs ++ '.' : drop (succ p) xs
```

# パート2

## 状況の記録

本当に $10^9$ 回繰り返す訳にもいかないし、そんなに状態数はないので、
途中に出現した状況を全て記録して、再度出現した状態のステップ数でループを見つけたい。

そのためには状況を表現するコンパクトなデータ型が必要。
`sample.txt` は10×10マス、`O`かどうかを1ビットに素直に割り当てる二進数100ビットは `Integer` が必要。
`input.txt` は100×100マス、同様にやって10^4ビット=1250バイト、`#` を飛ばすことで少しビット数が節約できるかもだけどオーダーとしては変化なし。

全ての `O` が東に寄っている、という性質を利用してもあまりうまくいきそうにないので、このまま進める。

## 回転

パート1の `score` を修正することで、リストの前が北、リスト列の前が西という地図が作れる。
これをベースに進めることもできるが、リスト操作はメモリに負荷がかかるので、mutable array に逃げる。

どちらの向きに傾いているか、を切り替えて、東西南北全てに石を寄せられるように作る。

行を寄せるには、下から始めて、`.` のマスが見つかるまで登っていく。
見つかったら、その次のマスから始めて、寄せられる `O` のマスが見つかるまで登っていく。
先に `#` に遭遇したら、そこまで全体を移動させて再起動する。
`O`が見つかったら、`.`マスに移し、移動先のマスを一つ上げて続ける。
（この、次の移動先のマスは必ず`.`である。）
捜索中に枠を踏みこたえたら終了する。

とりあえず1回転させてみる。

```haskell
part2 ls = runST $
  do
    fld <- newListArray bnds $ concat ls :: ST s (STUArray s (Int,Int) Char)
    forM_ [1 .. w] (\j -> shiftLine fld  (1,0) (1,j)) -- to North
    forM_ [1 .. h] (\i -> shiftLine fld (0,1)  (i,1)) -- to West
    forM_ [1 .. w] (\j -> shiftLine fld (-1,0) (h,j)) -- to South
    forM_ [1 .. h] (\i -> shiftLine fld (0,-1) (i,w)) -- to East
    chunksOf w <$> getElems fld
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    shiftLine fld dir p0 = loop1 p0
      where
        loop1 p | not (inRange bnds p) = return ()
        loop1 p = do
          cp <- readArray fld p
          if cp /= '.' then loop1 (add p dir) else loop2 p (add p dir)
        loop2 p q | not (inRange bnds q) = return ()
        loop2 p q = do
          cq <- readArray fld q
          case cq of
            '#' -> loop1 (add q dir)
            '.' -> loop2 p (add q dir)
            'O' -> writeArray fld p 'O' >> writeArray fld q '.' >> loop2 (add p dir) (add q dir)

add (a,b) (c,d) = (a+c,b+d)
```

```
ghci> runner "sample.txt" part2
[".....#...."
,"....#...O#"
,"...OO##..."
,".OO#......"
,".....OOO#."
,".O#...O#.#"
,"....O#...."
,"......OOOO"
,"#...O###.."
,"#..OO#...."]
```

よさそうだ。

## 記録のために

`fld` の内容を表す `Integer` を作る計算。

```haskell
    genKey fld = s2b <$> getElems fld

s2b :: String -> Integer
s2b = foldl' step 0 . filter ('#' /=)
  where
    step acc 'O' = acc .<<. 1 .|. 1
    step acc '.' = acc .<<. 1
```

`fld` の内容で、北の壁にかかる力を集計する計算。

```haskell
    score fld = sum . zipWith (*) [h, pred h ..] . map countO . chunksOf w <$> getElems fld

countO = length . filter ('O' ==)
```

## 記録しながら回す

記録が白紙の状態から始め、回転させてはビット列から生成した `Integer` にそのときのステップ数とスコアを記録する。

```haskell
import qualified Data.Map as M

part2 ls = runST $
  do
    fld <- newListArray bnds $ concat ls :: ST s (STUArray s (Int,Int) Char)
    loop fld 0 M.empty
  where
    loop fld cnt m = do
      key <- genKey fld
      if M.member key m then return (m M.! key, cnt) else do
        sco <- score fld
        forM_ [1 .. w] (\j -> shiftLine fld  (1,0) (1,j)) -- to North
        forM_ [1 .. h] (\i -> shiftLine fld (0,1)  (i,1)) -- to West
        forM_ [1 .. w] (\j -> shiftLine fld (-1,0) (h,j)) -- to South
        forM_ [1 .. h] (\i -> shiftLine fld (0,-1) (i,w)) -- to East
        loop fld (succ cnt) (M.insert key (cnt, sco) m)
```

やってみる。

```
ghci> test2
((3,69),10)
ghci> main2
((85,106719),107)
```

## 最終結果

開始から $a$ ステップと $b$ ステップで同じ状況になるということは、$b - a$ ステップ長のループがあるということ。
目標の $S = 10^9$ ステップ後の状況は、
初期状態から $a$ ステップ進み、残り $S - a$ ステップに関して $S - a \div b - a = q \dots r$ とすると
ループを $q$ 周してからさらに $r$ ステップ進んだ所になるので、結局 $a + r$ ステップ後と同じ。

`loop` を最終的な `m` も返させるように修正し、

```haskell
part2 ls = runST $
  do
    fld <- newListArray bnds $ concat ls :: ST s (STUArray s (Int,Int) Char)
    ((a, _), b, m) <- loop fld 0 M.empty
    let x = a + mod (10^9 - a) (b - a)
        s = head [s | (_,(c,s)) <- M.assocs m, c == x]
    return s
```

```
ghci> test2
64
ghci> main2
106689
```
