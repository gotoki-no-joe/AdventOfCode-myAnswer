# 入力

行をハイフンで区切って整数のペアにする。

```haskell
runner i f = do
  lus <- map parse . lines <$> readFile i
  print $ f lus

parse :: String -> (Int, Int)
parse l = (read as, read bs)
  where
    (as,_:bs) = break ('-' ==) l
```

# パート1

逆に、リストされていない開いている区間を取り出す一般化問題を考える。
ブラックリストのひとつの項目を、数直線上に貼られた紙テープの区間と考える。
重なったテープの厚みを正の方向になぞることを考えると、テープの左端ごとに1増えて、右端ごとに1減る。
この厚みが0であるような区間を取り出せばよい。

```haskell
build :: Int -> [(Int,Int)] -> [(Int,Int)]
build ub lus = loop ips
  where
-- 区間についてテープを貼る
-- 厚みの増減を記録する
-- 0までと、ubからには無限に1枚貼ってある
    dim0 = IM.fromListWith (+) $ concat [[(l, 1), (succ u, -1)] | (l,u) <- (succ ub, -1) : lus]
-- 増減0の点を除く
    dim = foldl (flip IM.delete) dim0 $ map fst $ filter ((0 ==) . snd) $ IM.assocs dim0
-- 0までは1、を初期値として、厚みの絶対値を累積で求める
    ips = zip (minBound : IM.keys dim) (scanl (+) 1 $ IM.elems dim)
-- 0なところから次の要素(0でない)までをペアにした、つまりlusの逆の整列された並びを返す
    loop ips =
      case dropWhile ((0 <) . snd) ips of
        [] -> []
        (a,_0) : (b, _p) : ips1 -> (a, pred b) : loop ips1
```

これがあれば、パート1は先頭の右側が答え。

```haskell
part1 ub = fst . head . build ub

test1 = runner "sample.txt" (part1 9)
main1 = runner "input.txt" (part1 4294967295)
```

# パート2

区間の長さを足し合わせればよい。

```haskell
part2 ub = sum . map (\(l,u) -> succ u - l) . build ub

test2 = runner "sample.txt" (part2 9)
main2 = runner "input.txt" (part2 4294967295)
```

