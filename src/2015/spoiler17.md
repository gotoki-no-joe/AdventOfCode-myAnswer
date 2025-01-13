# 入力

行ごとの数値を読み込むだけ。

```haskell
runner i f = readFile i >>= print . f . map read . lines
```

# パート1

## ナップザック問題

これはナップザック問題の一種。

入力の容器の個数は20個あるので、その総当たりの組み合わせは \\(2^20 = 1,048,576\\) とやや多い。

力任せにするなら、

- 以降のコップのリスト
- これまでに選んだコップの容量合計
- 以降のコップの容量合計

を引数にとり、

- 選んだ合計容量が目標ぴったりなら、一つ発見したと返す
- 越えていたら、それ以上調べずに引き返す
- まだ足りていないが、残りのコップを全て使っても目標に届かないなら、無理と引き返す
- 可能性が残っているなら、先頭のコップを使う場合と使わない場合の両方に再帰し、結果の合計を返す

という形に、枝刈りを意識した構造にしたい。

```haskell
part1a goal cups = iter rest cups (sum cups)
  where
    iter 0 _ _ = 1
    iter rest _ _ | rest < 0 = 0
    iter rest _ remain | rest > remain = 0
    iter _ [] _ = 0
    iter rest (x:xs) remain = iter rest xs remain1 + iter (rest - x) xs remain1
      where
        remain1 = remain - x

test1a = runner "test.txt" (part1a 25)
main1a = runner "input.txt" (part1a 150)
```

ほら、ただの全探索だと遅いでしょ、とやるつもりだったのだが、
2021年10月製造のcore i7ノートPCで、ノーストレスで答えが出てしまった。

枝刈りなしの版でもやってみる。

```haskell
part1b goal cups = iter 0 cups
  where
    iter acc [] = if acc == goal then 1 else 0
    iter acc (x:xs) = iter acc xs + iter (acc + x) xs
```

遅くはなったが、5秒は掛からない。

## 動的プログラミング

「これまでに調べたコップの組み合わせで、合計xリットルを作る方法の個数」を配列に持つ。
最初は「0リットルを作る方法は1通り」から、aリットルのコップを1つ追加するたびに、
xリットルを作る方法の個数をx+aリットルを作る方法に足し込む、を繰り返す。

```haskell
import Data.Array
import Data.List

part1c goal cups = arrN ! goal
  where
    arr0 = listArray (0, goal) $ 1 : repeat 0
    arrN = foldl' step arr0 cups
    step arr a = accum (+) arr $ zip [a .. goal] $ elems arr
```

最速で答えが出る。

# パート2

答えとして集める情報を、場合の数1、だけではなくて、
使う容器の個数ごとに分けて集計することになる。

集計は後回しにして、その容量を達成するひとつの組み合わせが使う容器の個数、のリストだけ集めて、
最後に、容器の使用数最小のものの個数を数えるやり方でしてみよう。

枝刈りなし全探索版、ただし結果を集める方法は `(++)` を回避するテクニックを使っている：

```haskell
part2a goal cups = (head ks, length ks) -- 使う容器の個数とその場合の数
  where
    iter acc cnt [] ans = if acc == goal then cnt : ans else ans
    iter acc cnt (x:xs) ans = iter acc cnt xs $ iter (acc + x) (succ cnt) xs ans
    ks = head $ group $ sort $ iter 0 0 cups []
```

上でも答えに易々と到達するので逆にがっくりするが、気をとりなおしてDP版：

```haskell
part2b goal cups = head $ filter ((0 <) . snd) $ assocs cnts
  where
    arr0 = listArray (0, goal) $ [0] : repeat []
    arrN = foldl' step arr0 cups
    step arr a = accum (++) arr $ zip [a .. goal] $ map (map succ) $ elems arr
    cnts = accumArray (+) 0 (0, length cups)
           [(k,1) | k <- arrN ! goal]
```
