# 入力

16ビット非負整数のビット単位論理演算を用いるので、それ用のライブラリを読み込んでおく。

```haskell
import Data.Word
import Data.Bits
```

行の形式は「(式) `->` (信号名)」となっている。
式には
- (信号)　これは演算を通さず、直結することを意味する
- (信号) (演算) (信号) の形式で、演算は `AND` `OR`
- (信号) (シフト演算) (数字列) の形式で、演算は `LSHIFT` `RSHIFT`
- `NOT` (信号)

のパターンがある。「（信号）」は、信号名もしくは数字列で16ビット整数を表す。

（例を鵜呑みにすると、`x AND y -> d` のように論理ゲートの入力はワイヤ名だけに見えるが、
実際には数が書いてあることもある。
また逆に、`123 -> x` のように数が信号に送り込まれる場合だけでなく、
`foo -> bar` のように論理ゲートなしで直結される信号もある。）

手を抜きつつ全ての場合を表現するには、次のようなデータ構造を定義できる。

```haskell
type Instr =
  ( String   -- 出力先の信号名
  , ( String   -- 論理ゲートの名前 "-"（直結）,AND,OR,LSHIFT,RSHIFT,NOT
    , [String] -- 入力元の信号名または数字列、0～2要素
    )
  )
```

もっと真面目に、次のような代数的データ型で表現することもできるが、もうこれでいいだろう。

```haskell
type Instr = (String, LHS) -- 出力先の信号名、左辺の式

data SIGNAL = LIT Word16 | WIRE String

data LHS
  = ID SIGNAL
  | AND SIGNAL SIGNAL
  | OR  SIGNAL SIGNAL
  | LSHIFT SIGNAL SIGNAL
  | RSHIFT SIGNAL SIGNAL
  | NOT SIGNAL
```

行を読み込む。

```haskell
runner i f = readFile i >>= print . f . map parse . lines

parse :: String -> Instr
parse l = (last ws, lhs)
  where
    ws = words l
    ws0:ws1:ws2:_ = ws
    lhs = case length ws of
      3 -> ("-", [ws0])       -- 123 -> x, hoge -> a
      4 -> (ws0, [ws1])       -- NOT x -> h
      5 -> (ws1, [ws0, ws2])  -- x AND y -> d, x OR y -> e, x LSHIFT 2 -> f, y RSHIFT 2 -> g
```

# パート1

信号名に対して、その値を左辺に基づいて計算する。
そのとき、左辺に現れる信号の値が必要になる。
これを再帰関数で素朴に実装すると止まらない。配線がどこかでループしているようだ。

答えが求まるということは、ループがあるにもかかわらず、信号の値は発振するようなことはなく安定するのだろう。
一度計算した信号の値を再計算しないように、メモ化再帰と同等の、`Map`を用いた計算を行う。

```haskell
import qualified Data.Map as M

buildMap :: [Instr] -> M.Map String Word16
buildMap is = m
  where
    m = M.fromList [(sig, eval lhs) | (sig, lhs) <- is]
    eval (op, args) = op2f (head op) $ map readSig args
    op2f '-' [x]    = x
    op2f 'A' [x, y] = x .&. y
    op2f 'O' [x, y] = x .|. y
    op2f 'L' [x, y] = shiftL x (fromIntegral y) -- シフト量引数は Int
    op2f 'R' [x, y] = shiftR x (fromIntegral y)
    op2f 'N' [x]    = complement x
    readSig arg
      | all isDigit arg = read arg
      | otherwise       = m M.! arg
```

サンプルデータには `a` という信号がないので、構築したマップそのものを表示して動作を確認する。

```
ghci> runner "test.txt" buildMap
fromList [("d",72),("e",507),("f",492),("g",114),("h",65412),("i",65079),("x",123),("y",456)]
```

本番データは巨大なので、`a` の値だけ求めよう。

```haskell
part1 is = buildMap is M.! "a"

main1 = runner "input.txt" part1
```

# パート2

`buildMap` の `readSig` に細工をするのが早いのだが、それもダサいので、
真面目に `[Instr]` を差し替える形でする。

```haskell
part2 is = buildMap m1 M.! "a"
  where
    m1 = [if sig == "b" then (sig, lit3176) else i | i@(sig, lhs) <- is]
    lit3176 = ("-",["3176"])

main2 = runner "input.txt" part2
```
