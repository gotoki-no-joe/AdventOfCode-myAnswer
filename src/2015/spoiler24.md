<details><summary>解説</summary><div>

入力の行数から、包みの個数はほどほどで、部分集合をビット表現するのに整数で足りる。
包みの部分集合について、その合計重量をキー、部分集合のビット表現のリストを値とするマップを、
「0個の取り合わせは0」から始めて、荷物をひとつずつ追加することで、全ての組み合わせの合計重量を求める。
ただし、総重量の1/3を超えるものは不要なので作らないようにする。
このマップの、総重量の1/3になる選択群から、条件を満たすものを選択する。

```haskell
import qualified Data.IntMap as IM
import Data.Bits
import Data.List

compute1 xs = ...
  where
-- 重量の1/3、目標値
    w3 = div (sum xs) 3
-- 重量0にする方法は荷物なし、のみの初期値から、荷物を一つずつ追加
    im = foldl' step (IM.singleton 0 [0]) $ zip xs [0..]
-- i個めの重量xの荷物を追加したパターンをマップに追加
    step im (x, i) = IM.unionWith (++) im $ IM.fromAscList $
      [(w1, map (bit i .|.) bs) | (w,bs) <- IM.assocs im, let w1 = w + x, w1 <= w3]
-- 重量w3な全ての組み合わせが得られた
    bs = im IM.! w3 :: [Int]
```

見つかった組み合わせを、その要素数順、次に量子もつれの順で整列する。
これを小さい方から試して、3つの組み合わせで、互いに重なりないようなものが、探したいものである。

```haskell
compute1 xs = ans
  where
    ...
-- 品数はpopCountで数える、量子もつれは重量の積、前者優先で昇順に整列
    cands = sort [(popCount b, qe, b) | b <- bs, let qe = product [x | (i,x) <- zip [0..pred num] xs, testBit b i]]
-- のこり2つの荷物について、使う荷物が重複しない組み合わせを探す
    ans = head
      [ qe
      | (_,qe,b1):cands1 <- tails cands
      , (_,_ ,b2):cands2 <- tails cands1, b1  .&. b2 == 0, let b12 = b1 .|. b2
      , (_,_ ,b3)        <-       cands2, b12 .&. b3 == 0
      ]

main = readFile "input.txt" >>= print . compute1 . map read . lines
```

# パート2

分割が4つになるだけで、やることは変わらない。

```haskell
compute2 xs = ans
  where
    w4 = div (sum xs) 4
    im = ...
    step im (x, i) = ...
    bs = im IM.! w4 :: [Int]
    cands = ...
    ans = head
      [ qe
      | (_,qe,b1):cands1 <- tails cands
      , (_,_ ,b2):cands2 <- tails cands1, b1   .&. b2 == 0, let b12  = b1  .|. b2
      , (_,_ ,b3):cands3 <- tails cands2, b12  .&. b3 == 0, let b123 = b12 .|. b3
      , (_,_ ,b4)        <-       cands3, b123 .&. b4 == 0
      ]
```
