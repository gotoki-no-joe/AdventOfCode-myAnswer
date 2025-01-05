# 入力

文字の二次元配列にするくらいしか思いつかない。

```haskell
import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls
  print $ f fld

test1 = runner "samp1.txt" part1
test2 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

part1 fld = ...
  where
```

# パート1

昨日のヌルさが嘘のようなややこしさ。

マスについて、東西南北の向きにそれぞれ通じているかを返す述語を定義する。

```haskell
    toNorth p = elem (fld ! p) "|LJ"
    toSouth p = elem (fld ! p) "|7F"
    toEast  p = elem (fld ! p) "-LF"
    toWest  p = elem (fld ! p) "-J7"
```

位置から東西南北に移動した位置を求める関数を定義する。

```haskell
    goNorth (i,j) = (pred i, j)
    goSouth (i,j) = (succ i, j)
    goEast  (i,j) = (i, succ j)
    goWest  (i,j) = (i, pred j)
````

出発地点 `S` について、`input.txt` 他をみて、周囲で接続しているマスは2つだけで、ハズレの経路はないことを確認する。
なので `S` に限り、周囲のマスを見て移動可能な方向を判断する。

普通にBFSで探索を進めと、パイプの形状から枝分かれもないので、最前線は常に2点だけになる。
なので、`S`から一歩出たところから始め、1ステップ前にはどこに居たかを覚えていれば、
そうでない方向として進むべき方向は一意に定まる。
終わりは、両者が同時に目的地に到達する場合と、両者がばったり出会う場合の両方が考えられる。

```haskell
import Data.Tuple

-- エージェントを、現在の位置と一つ前の位置の対で表す
type POS = (Int,Int)
type Agent = (POS,POS)

-- Sの位置を探す
    posS = head [p | (p,'S') <- assocs fld]

-- Sの隣接マスで、Sの方に移動できるマスがエージェントの初期位置
    bnds = bounds fld
    [a1,a2] = [(p, posS) | let p = goNorth posS, inRange bnds p, toSouth p]
           ++ [(p, posS) | let p = goSouth posS, inRange bnds p, toNorth p]
           ++ [(p, posS) | let p = goEast  posS, inRange bnds p, toWest  p]
           ++ [(p, posS) | let p = goWest  posS, inRange bnds p, toEast  p]

-- マスから行ける方角で、一つ前の位置と違う方へ、エージェントを一歩進める
    stepAgent (pos, prev) = (new, pos)
      where
        new = head $
              [p | toNorth pos, let p = goNorth pos, p /= prev]
           ++ [p | toSouth pos, let p = goSouth pos, p /= prev]
           ++ [p | toEast  pos, let p = goEast  pos, p /= prev]
           ++ [p | toWest  pos, let p = goWest  pos, p /= prev]

-- 二人が同じ位置に到達するか、すれ違うまで歩数を数える。後者の場合1減らす
    ans = loop 1 a1 a2
    loop cnt a1 a2
      | fst a1 == fst a2 = cnt -- 同じ位置に到達
      | swap a1 == a2    = negate cnt -- すれ違った（本当はpred cntでよい）
      | otherwise = loop (succ cnt) (stepAgent a1) (stepAgent a2)
```

4方向に関する操作をもっと共通化するべきとも思うが、動いたからヨシ！

# パート2

原則として、壁を奇数回乗り越えた位置のマスが内側にあるマスなので、横になぞって個数を数えればよい。
ただし、単に「壁のマスを数える」だと、`L-7` のような横方向のマス、曲がり角のマスで混乱する。
左から右へ走査するとして、次のように考える：

- `|` 壁を1回乗り越えた
- `LF` 横に走る壁に乗り上げた。壁の内側に居たならカウントを終了する。
- `-` 壁に乗り上げ中
- `7J` 横に走る壁が切れた。壁の外側に居たとき内側に入ったのでカウント開始。

また、ループを構成する以外の壁は全て `.` に置き換えておく。
`S` も、前後のマスから対応するマスに差し替える。

パート1と共通部分が多いので、コードの追加で対応する

```haskell
import Data.List.Split

part12 fld = (ans, ans2)
  where
-- エージェントの軌跡をとり、壁の位置を全て集める
    walls = loop2 a1 a2
    loop2 a1 a2
      | fst a1 == fst a2 = [fst a1]
      | swap a1 == a2    = [] -- 不要だけど
      | otherwise = fst a1 : fst a2 : loop2 (stepAgent a1) (stepAgent a2)

-- 壁の位置はfldのまま、`S`の位置は妥当な壁に、その他の位置は `.` に置き換えたマップを作る
    fld2 = accumArray (flip const) '.' bnds $
           (posS, charS) : [(p, fld ! p) | p <- walls]

-- `S`の位置の壁は、a1とa2の位置に行けるもの
    charS
      | isN, isE = 'L'
      | isN, isS = '|'
      | isN, isW = 'J'
      | isE, isS = 'F'
      | isE, isW = '-'
      | isS, isW = '7'
      where
        a1a2 = map fst [a1, a2]
        isN = elem (goNorth posS) a1a2
        isS = elem (goSouth posS) a1a2
        isE = elem (goEast  posS) a1a2
        isW = elem (goWest  posS) a1a2

-- 行ごとに数えて合計する
    (_,(_,w)) = bnds
    ans2 = sum $ map countLine $ chunksOf w $ elems fld2

-- 行について数える
  -- 壁の外
    countLine "" = 0
    countLine (x:xs)
      | elem x "|7J" = countIn xs
      | otherwise    = countLine xs
  -- 壁の中にいる
    countIn "" = 0
    countIn (x:xs)
      | elem x "|LF" = countLine xs
      | otherwise    = succ $ countIn xs
```

はい、嘘解答でした。
壁が `F--J` とか `L--7` なら確かに越えたけれど、
`F--7` ではその壁に対して同じ側のままなのでカウントは元に戻る必要がある！

- 壁に乗り上げる前は壁の中外どっちだったか
- 壁は北から生えていたか南からだったか

の4とおりの場合について正しく対応する必要があるように見えるが、
結局次に進むのは `countLine` か `countIn` のいずれかなので、

- 壁が南に抜けたときに進むのは `countIn` の方か

だけ持たせておけばよい。

```haskell
-- 行について数える
  -- 壁の外
    countLine "" = 0
    countLine ('|':xs) = countIn xs
    countLine ('L':xs) = ontheWall True  xs
    countLine ('F':xs) = ontheWall False xs
    countLine ('.':xs) = countLine xs
  -- 壁の中にいる
    countIn "" = 0
    countIn ('|':xs) = countLine xs
    countIn ('L':xs) = ontheWall False xs
    countIn ('F':xs) = ontheWall True  xs
    countIn ('.':xs) = succ $ countIn xs
  -- 壁の上
    ontheWall goInif7 ('-':xs) = ontheWall goInif7 xs
    ontheWall goInif7 ('7':xs) = if goInif7 then countIn xs else countLine xs
    ontheWall goInif7 ('J':xs) = if goInif7 then countLine xs else countIn xs
```
