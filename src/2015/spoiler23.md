（偶数を）半分にするとか（奇数を）3倍して1を足すとか、
どこかで聞いたような操作（コラッツ数列）しかないコンピュータだなぁ、と。

# 入力

命令を観察すると、指定したレジスタの内容を演算して更新するものと、
レジスタの内容に関する条件によってジャンプをするもののふたつに分かれていることがわかる。
（無条件ジャンプも「常に成立する」条件が指定されていると見なせる。）

後で、レジスタの実体は配列で実現すると想定して、レジスタa,bは番号0,1で呼ぶことにする。
これらを踏まえて、命令を表す代数的データ型を定義する。

```haskell
type Reg = Int
data Inst = Ialu (Int -> Int) Reg | Ijmp (Int -> Bool) Reg Int
```

命令一行を読み込む関数を定義する。
このとき命令に応じて妥当な関数を設定する。

```haskell
parse :: String -> Inst
parse xs =
  case words xs of
    ["hlf", reg] -> Ialu (flip div 2) (regp reg)
    ["tpl", reg] -> Ialu (3 *)        (regp reg)
    ["inc", reg] -> Ialu succ         (regp reg)
    ["jmp", ofs] -> Ijmp (const True) undefined (readofs ofs) -- 無条件
    ["jie", reg, ofs] -> Ijmp even   (regp reg) (readofs ofs)
    ["jio", reg, ofs] -> Ijmp (1 ==) (regp reg) (readofs ofs)
  where
    regp ('a':_) = 0             -- 後ろのコンマを無視できるように
    regp ('b':_) = 1
    readofs ('+':cs) = read cs   -- 数値は'+'付きだとreadできない
    readofs cs = read cs
```

プログラムはランダムアクセスするので、配列に格納しておこう。

```haskell
import Data.Array

runner i f = do
  src <- map parse . lines <$> readFile i
  let prog = listArray (1, length src) src
  print $ f $ prog

# パート1

プログラムカウンタが逸脱するまで命令を実行し続けるCPUエミュレータを作る。

```haskell
exec :: Array Int Inst -> Int -> Array Reg Int -> Array Reg Int
exec prog pc regF
  | not $ inRange (bounds prog) pc = regF
  | otherwise =
    case prog ! pc of
      Ialu f r -> exec prog (succ pc) (regF // [(r, f $ regF ! r)])
      Ijmp p r ofs | p (regF ! r) -> exec prog (pc + ofs) regF
                   | otherwise    -> exec prog (succ pc)  regF
```

入力を読み込み、レジスタを0に初期化して実行し、レジスタの最終状態を観察する。

```haskell
part1 prog = exec prog 1 (listArray (0,1) [0,0])
```

# パート2

レジスタの初期値を変えて実行するだけ。

```haskell
part1 prog = exec prog 1 (listArray (0,1) [1,0])
```
