# 入力

左側の地図は文字列、右側の数値列は読み取り、対にする。

```haskell
import Data.List.Split

runner i f = do
  ps <- map parse . lines <$> readFile i
  print $ f ps

parse :: String -> (String, [Int])
parse l = (l1, ks)
  where
    l1:l2:_ = words l
    ks = map read $ wordsBy (',' ==) l2

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
```

# パート1

行ごとに場合の数を数えて総和をとる。

```haskell
part1 = sum . map score
```

一つの行について、文字列を順に調べていく。
同時に、数値列をどこまで消費したかも保持する。

- `.` のとき、単に読み飛ばす。
- `#` のとき、スプリング列が始まる。数値列の値だけ、ここから`#`または`?`が並んでおり（つまり`.`がない）、さらに、そこで文字列が終了するか、次に空白がある、つまり`#`でないことが必要である。そうでない場合、矛盾するので放棄する。また、「次に文字列があるか空白がある」を判定する代わりに、文字列の末尾に `.` を追加しておくことで、「空白がある」だけに限定できる。
- `?` のとき、どちらとも解釈してよいので、まず必ず空白としての解釈を試みる。さらにスプリングとして解釈可能ならそれも行い、両者の和をとる。

```haskell
score :: ([Char], [Int]) -> Int
score (l0, ks) = recur l ks
  where
    l = l0 ++ "." -- 番兵
    recur "" (_:_) = 0 -- 文字を使い切って数が残ったら失敗
    recur cs [] = if notElem '#' cs then 1 else 0 -- 数を使い切って#が残っていなければ成功
    recur ccs@(c:cs) kks@(k:ks) =
      case c of
        '.'             -> dotCase   -- '.' はただ読み飛ばす
        '#' | cond      -> sharpCase -- ここは成功、次の1文字も除いて次へ
            | otherwise -> 0         -- 連続kの#と続く空白を確保できない
        '?' | cond      -> dotCase + sharpCase
            | otherwise -> dotCase
      where
        dotCase = recur cs kks
        sharpCase = recur (tail cs2) ks
        (cs1, cs2) = splitAt k ccs
        cond = notElem '.' cs1 && -- k個までは`.`がなく
               not (null cs2) &&  -- 続きが残っていて
               head cs2 /= '#'    -- その先頭は`.`になれる
```

# パート2

説明どおりに文字列と数値列を5倍してそのまま`score`にかけると、`?`のたびに再帰呼び出しが倍になるので計算量が爆発する。

DPの登場する典型的な局面。

`recur` の引数を文字列そのもの、数値列そのものから、その列のいくつを消費済みか、という数に差し替える。

```haskell
import Data.Array

score2 :: ([Char], [Int]) -> Int
score2 (l0, ks) = recur 0 0
  where
    llen = succ $ length l0
    carr = listArray (0, pred llen) $ l0 ++ "."
    klen = length ks
    karr = listArray (0, pred klen) ks

    recur i j
      | i == llen, j < klen = 0    -- 文字を使い切って数が残ったら失敗
      | j == klen, noSharp  = 1    -- 数を使い切って#が残っていなければ成功
      | j == klen      = 0         -- あれば失敗
      | c == '.'       = dotCase   -- '.' はただ読み飛ばす
      | c == '#', cond = sharpCase -- ここは成功、次の1文字も除いて次へ
      | c == '#'       = 0
      | c == '?', cond = dotCase + sharpCase
      | c == '?'       = dotCase
      where
        noSharp = and [carr ! ii /= '#' | ii <- [i .. pred llen]]
        c = carr ! i
        k = karr ! j
        dotCase   = recur (succ i) j
        sharpCase = recur (i + succ k) (succ j)
        ik = i + k
        cond = ik < llen &&
               and [carr ! ii /= '.' | ii <- [i .. pred ik]] &&
               carr ! ik /= '#'

part1a = sum . map score2
```

パート1が正しく計算できることを確認する。

次に、`recur i j` の内容を `arr ! (i,j)` に持つ配列を定義することで、これをDPに変える。

```haskell
score3 :: ([Char], [Int]) -> Int
score3 (l0, ks) = arr ! (0, 0)
  where
    ...

    bnds = ((0,0), (llen, klen))
    arr = listArray bnds $ map recur $ range bnds
    recur (i, j)
      | ...
      where
        ...
        dotCase   = arr ! (succ i, j)
        sharpCase = arr ! (i + succ k, succ j)

part1b = sum . map score3
```

変更はごくわずかで済む。
パート1がはるかに短い時間で解けることを確認する。

あとは、パート2の設定にデータを引き延ばして与えればよい。

```haskell
import Data.List

extend (l, ks) = (l5, ks5)
  where
    l5 = intercalate "?" $ replicate 5 l
    ks5 = concat $ replicate 5 ks

part2 = sum . map (score3 . extend)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2
```
