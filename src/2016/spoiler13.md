# 準備

まず、迷路の壁と行ける場所を判別する関数を作る。

```haskell
isOpen :: Int -> (Int, Int) -> Bool
isOpen base (x,y) = even $ popCount $ x * (x + 3 + y + y) + y * succ y + base
```

動作確認

```
ghci> putStr $ unlines $ [ [if isOpen 10 (j,i) then '.' else '#' | j <- [0 .. 9]] | i <- [0 .. 6]]
.#.####.##
..#..#...#
#....##...
###.#.###.
.##..#..#.
..##....#.
#...##.###
```

問題文の地図と同じものが作れた。

# パート1

day11で作った`bfs`を使えば距離はすぐ測れる。

```haskell
gen :: Int -> (Int,Int) -> [(Int,Int)]
gen base (x0,y0) =
  [ (x,y)
  | (x,y) <- [(pred x0,y0),(succ x0,y0),(x0, pred y0),(x0, succ y0)]
  , x >= 0, y >= 0
  , isOpen base (x,y)]

sample = bfs [(1,1)] (gen 10) (7,4)

main1 = bfs [(1,1)] (gen 1352) (31,39)
```

# パート2

上で使った `bfs` はこういう設定では使えない。

ゴールの代わりにステップ数を引数で渡し、それだけ実行したら
調査済み座標集合そのものを返して終わりにしよう。

```haskell
bfs1 :: Ord a => [a] -> (a -> [a]) -> Int -> S.Set a
bfs1 inits gen steps = loop steps st0 st0
  where
    st0 = S.fromList inits
    loop cnt visited sts
      | S.null sts = visited  -- 行き詰まった
      | cnt == 0   = visited -- 到達した
      | otherwise  = loop (pred cnt) visited1 sts1
      where
        sts1 = S.fromList [st1 | st <- S.elems sts, st1 <- gen st, S.notMember st1 visited]
        visited1 = S.union visited sts1

main2 = S.size $ bfs1 [(1,1)] (gen 1352) 50
```
