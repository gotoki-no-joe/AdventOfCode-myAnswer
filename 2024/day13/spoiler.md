# 入力

不要な文字が多い書式でぎょっとする。
エディタで検索をかけてみて、マイナス文字はないことを確認する。負の値はない。

数字以外の文字は全て空白に置き換えてしまえば、`words` で数字の並びだけ取り出せる。
間違えて `filter isDigit` すると全て詰まってしまうので注意。
さらに、ひとつのデータが6個の数字からなるので、`Data.List.Split.chunksOf` で切り飛ばしておこう。

もう少しマニュアルを調べると、`Data.List.Split.wordsBy (not . isDigit)` で前半が済むとわかると今知った。

```haskell
import Data.Char
import Data.List.Split

runner i f = readFile i >>= print . f . chunksOf 6 . map read . wordsBy (not . isDigit)

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1
```

# パート1

ひとつのデータについて、解があれば返す関数が問題の中核をなす。
`Data.Maybe.mapMaybe` は、`map`した結果の`Just`だけ並べたリストを作る。
`catMaybes . map f` と書いたら IDE に突っ込まれて今知った。

```haskell
import Data.Maybe

part1 = sum . mapMaybe compute1

compute1 :: [Int] -> Maybe Int
compute1 [ax, ay, bx, by, px, py] = ...
```

ボタンAを `i` 回押すとXに `i * ax` Y に `i * ay` 動くので、これが `px`, `py` を越えない範囲で調べる。
残りの移動をボタンBで行うためには、X方向もY方向も割り切れて、かつ回数が同じになる必要がある。
そのような候補を全て調べ、コストの最小値を選ぶ。
一つもない場合もあるので注意。

```haskell
compute1 [xa, ya, xb, yb, px, py]
  | null cands = Nothing
  | otherwise  = Just $ minimum cands
  where
    cands = [ 3 * i + j
            | i <- [0 .. min (div px xa) (div py ya)]
            , let (j, r) = divMod (px - xa * i) xb, r == 0
            , ya * i + yb * j == py
            ]
```

ボタンBを押す回数`j`を、X方向だけで調べ、Y方向には矛盾がないことを確認するだけにした。

パート1はこれで完成。

# パート2

`px`, `py` が大きくなる以外はやることは変わらないので、とりあえずやってみる。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

part2 = sum . mapMaybe (compute1 . offset)

offset args = as ++ map (10000000000000 +) bs
  where
    (as,bs) = splitAt 4 args
```

…ビクともしない。
最初のサンプルでいうと、`i` のスキャン上限は `div (10000000000000 + 8400) 94 = 106382978812`
$\fallingdotseq 10^{11}$ で、今の計算機で総当たりするのはちょっと現実的でない。

## 数学のお時間

つまり、下の連立方程式の解を求めたい。

$ax \cdot i + bx \cdot j = px$  
$ay \cdot i + by \cdot j = py$  

$i$ の係数が同じになるように掛けて

$ay \cdot ax \cdot i + ay \cdot bx \cdot j = ay \cdot px$  
$ax \cdot ay \cdot i + ax \cdot by \cdot j = ax \cdot py$  

辺どうし引いて

$(ay \cdot bx - ax \cdot ay) \cdot j = ay \cdot px -  ax \cdot py$

$ay \cdot bx - ax \cdot ay = \det \neq 0$ を仮定して、  
$j = (ay \cdot px -  ax \cdot py) / (ay \cdot bx - ax \cdot ay)$  
$i = (px - bx \cdot j)/ax$  
結局これが唯一の解なので、割り切れて整数であることと、 $i,j \geq 0$ であることをダメ押しで確認して終わり。

行列式が0になる場合、連立方程式は係数が同じ比になっていて、  
$ax / ay = bx / by = r$ となる。
このとき $px / py = r$ でもあるなら、これは単なる直線の式で、その上には無数の点がある。
（ただし、格子点を通るとは限らない。）
$px / py \neq r$ なら矛盾しており、解は全くない。

$px / py = r$ の場合に、格子点を探してその中からコスト最小のものを選択するのは振り出しに戻ったような感じ。

しかしここで、実は $px, py$ は元々せいぜい 20000 程度の値（`input.txt`を覗いて確認しよう）で、
これに 10000000000000 を足した、どちらもほぼ 10000000000000 に近い値であることを思い出そう。
そのような値どうしで $px / py = r$ となるには、 $px = py, r = 1$ とするしかない。
すると $ax = ay, bx = by$ と単純化されるが、とはいえ途方もない探索範囲がある。
万一そのような場合に遭遇したらエラーを出すように仕込むことにしよう。

## プログラミングのお時間

```haskell
part2 = sum . mapMaybe compute2

compute2 :: [Int] -> Maybe Int
compute2 args@[xa, ya, xb, yb, px0, py0]
  | det == 0, px == py = error $ show args -- 緊急事態、要対応
  | det == 0           = Nothing           -- 解なし
  | isOK               = Just $ 3 * i + j  -- 唯一解
  | otherwise          = Nothing           -- 自然数解でない
  where
    px = px0 + 10000000000000
    py = py0 + 10000000000000
    det = xb * ya - yb * xa
    (j, r) = divMod (ya * px - xa * py) det
    (i, s) = divMod (px - xb * j) xa
    isOK = r == 0 && s == 0 && i >= 0 && j >= 0
```

緊急事態は発生することなく答えが出た。そういう入力は混じらないように配慮されていると思われる。
