# 入力

行の並び以上の分析は後に任せる。

```haskell
runner f = readFile "input.txt" >>= print . f . lines
```

# パート1

ロボはチップが手許に2つ届くと、指示に従って二手に分ける。
この情報はロボごとに指定されるものなので、ロボの番号をキー、
小さい方と大きい方の送り先を値とする `IntMap (Int,Int)` にこれを保持する。

行の内容は先頭の一文字で区別がつく。

```haskell
import qualified Data.IntMap as IM

part1 ls = ...
  where
    gives = IM.fromList
      [ (botid, (lowid, hiid))
      | 'b':l <- ls, let ws = words l
      , let botid = read $ ws !! 1 :: Int
      , let lowid = roro (ws !! 5)  (ws !! 6)  :: Int
      , let hiid  = roro (ws !! 10) (ws !! 11) :: Int ]
```

`input.txt` を調べると、ロボと製品箱の番号のどちらも0で始まるため、
ロボ 0,1,2,… を整数 0,1,2,… で、
箱 0,1,2,… を整数 -1,-2,-3,… で表すことにする。

```haskell
    roro "bot"    x = read x
    roro "output" x = - (succ $ read x)
```

チップがどこにあるかが、状況の進行によって変化する。その変化を追跡する必要がある。
製品箱は、複数のチップが投入される可能性がある。
ロボは、2つのチップを持つと指示に従って誰かにそれらを渡す。
また、`input.txt` の指示に従って、このシステムにチップが投入される。

ロボおよび製品箱が持っているチップを `IntMap [Int]` で表現する。
投入予定でまだ処理されていないチップのキュー的なものを `[Int]` で保持する。
ロボがチップを二つ揃え、誰かに渡すときも一旦このキューに入れる。

なお、パート1で要求されている答えは最終状態でなく途中のことなので、
ロボが2つのチップを誰かに渡した後、自分の手持ちチップを空にはせずそのままにしておく。
これは、ロボが動作するのはそれぞれ一度きり、という勝手な想定をしている。

また、チップの投入順序も結果に影響はないと仮定して、
「キュー的なもの」は真面目にFIFOを使うのでなくリストによるスタックで横着する。

以上の検討に従い、チップの最終状態を求める関数が定義できる。

まず、`input.txt` から、キューの初期値を作る。

```haskell
    chipins =
      [ (val, dest)
      | 'v':l <- ls, let ws = words l
      , let val = read $ ws !! 1 :: Int
      , let dest = roro (ws !! 4) (ws !! 5) :: Int]
```

チップのやりとりをシミュレーションする。

```haskell
    imZ = loop IM.empty chipins
    loop im [] = im
    loop im ((val, dest):chips) =
      case im1 IM.! dest of
        [v1, v2] | dest >= 0 -> loop im1 ((min v1 v2, lowid) : (max v1 v2, hiid) : chips)
        _ -> loop im1 chips
      where
        im1 = IM.insertWith (++) dest [val] im
        (lowid, hiid) = gives IM.! dest
```

`imZ` の 61 と 17 を持つキーが答えである。

```haskell
    part1ans = [rid | (rid, chips) <- IM.assocs imZ, elem 61 chips, elem 17 chips]
```

ただし、勝手な仮定が逸脱していないこと、すなわち、`imZ` の（うち、ロボの）リストは長さ2を越えないことを確認する。

```haskell
part1 ls
  | any ((2 <) . length . snd) $ IM.assocs imZ = error $ show imZ -- 3以上入れられた誰かがいる
  | otherwise = part1ans
  where
    ...
```

問題なかったようだ。

# パート2

その答えはもうほとんど出ている。
`part1` に追加するだけで済ませる。

```haskell
part12 ls = (part1ans, part2ans)
  where
    out012 = map (imZ IM.!) [-1, -2, -3]
    part2ans = (out012, product $ concat out012)
```

確かに箱には一つずつチップが入っていた。
