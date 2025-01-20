# 素直に考える

名物ヘンテコCPUエミュレータ問題。

命令を表現する代数的データ型を設計する。

`cpy x y` 命令は、`y`はレジスタ名、`x`はレジスタ名の場合と定数の場合がある。
このアドレッシングモードの違いを、命令の違いとして定義する。

```haskell
data Instr                  -- 命令
    = CpyI Int  Char        -- 定数ロード
    | CpyR Char Char        -- レジスタ間転送
    | ...
```

`inc x`, `dec x` 命令は、レジスタを指定する。

```haskell
    | Inc Char
    | Dec Char
```

`jnz x y` 命令は、`y`は定数、`x`はレジスタ名の場合と定数の場合がある。
定数のとき、それは0かそうでないかもう確定しているので、無条件ジャンプまたはNOPと見なせる。
実行時に判断する必要はない。

```haskell
    | Jnz Char Int
    | J Int
    | NOP
```

## 入力

行を `Instr` で読み込み、その配列をプログラムとして読み込む。

```haskell
import Data.Array

parse l =
  case words l of
    ['c':_, x, y:_] | isReg x -> CpyR (head x) y
                  | otherwise -> CpyI (read x) y
    ['i':_, x:_]              -> Inc x
    ['d':_, x:_]              -> Dec x
    ['j':_, x, y]   | isReg x -> Jnz (head x) (read y)
                | read x == 0 -> NOP
                  | otherwise -> J (read y)

isReg (c:cs) = elem c "abcd" && null cs

runner i f = do
  is <- map parse . lines <$> readFile i
  let prog = listArray (1, length is) is
  print $ f prog
```

## パート1

レジスタの内容は配列に保持し、命令ポインタが逸脱するまで命令を実行し、
レジスタ`a`の最終値を返す。

```haskell
exec prog xs = step (fst bnds) (listArray ('a','d') xs)
  where
    bnds = bounds prog
    step ip regs
      | not $ inRange bnds ip = regs ! 'a'
      | otherwise =
          case prog ! ip of
            CpyI i c -> loop (succ ip) (regs // [(c, i)])
            CpyR x y -> loop (succ ip) (regs // [(y, regs ! x)])
            Inc x    -> loop (succ ip) (regs // [(x, succ $ regs ! x)])
            Dec x    -> loop (succ ip) (regs // [(x, pred $ regs ! x)])
            Jnz x y  -> loop (if regs ! x /= 0 then ip + y else succ ip) regs
            J   y    -> loop (ip + y) regs
            NOP      -> loop (succ ip) regs

part1 prog = exec prog [0,0,0,0]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1
```

結果。

```
ghci> test1
42
(0.00 secs, 109,152 bytes)
ghci> main1
317993
(1.90 secs, 713,360,256 bytes)
ghci> main2
9227647
(58.78 secs, 20,697,357,904 bytes)
```

当時の計算機だと、もう少し我慢ならない時間がかかった。

# トランスパイル

4つのレジスタの値をどうこうしては、次のアドレスの実行に移る、というのが個々の命令の動作。
そのような動作をする、Haskellの一つの関数を作ることはできないか。
例えば問題文サンプル（アドレスを付記した）

```
1: cpy 41 a
2: inc a
3: inc a
4: dec a
5: jnz a 2
6: dec a
```

に対して、

```haskell
sample = f1 where
  f1 a b c d = f2 41 b c d -- アドレス1の命令は、レジスタaを41にして、アドレス2へ進む
  f2 a b c d = f3 (succ a) b c d -- アドレス2の命令は、レジスタaを1増やして、アドレス3へ進む
  f3 a b c d = f4 (succ a) b c d -- 同上
  f4 a b c d = f5 (pred a) b c d
  f5 a b c d = (if a /= 0 then f7 else f6) a b c d -- aが非0なら2つ先へ、さもなくば次へ
  f6 a b c d = f7 (pred a) b c d
  f7 a _ _ _ = a                 -- プログラムを外れたら、aの最終結果を返す
```

を `sample 1 0 0 0 0` で呼び出せば、`a` の最終結果42が返る。
分岐命令の分岐先を吟味して、ジャンプでプログラムの範囲を大幅に外れて終了する動作はない、
という追加の知識で、`f7` だけを終了の場合にした。

これを当時は手作りしたが、プログラムを読み込んで自動生成、
あるいはTemplate Haskellでコンパイル時に生成しつつ実行することも可能だろう。

```haskell
generator i = readFile i >>= putStr . part3 . map parse . lines

part3 prog = unlines $ zipWith f [0 ..] prog ++ [trailer]
  where
    regf t rep = [if t == s then rep else [s] | s <- "abcd"]
    f i (CpyI v t) = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t (show v)
    f i (CpyR u t) = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t [u]
    f i (Inc t)    = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t ("(succ " ++ t : ")")
    f i (Dec t)    = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t ("(pred " ++ t : ")")
    f i (Jnz t y)  = unwords $ ["  f"++ show i, "a b c d = (if ", t : " /= 0 then f" ++ show (i + y), "else f" ++ show (succ i), ") a b c d"]
    f i (J y)      = unwords $ ["  f"++ show i, "a b c d = f"++ show (i + y), "a b c d"]
    f i  NOP       = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i), "a b c d"]
    trailer = "  f" ++ show (length prog) ++ " a _ _ _ = a"
```

変換で出力された関数を `input` として試す。

```
ghci> input 0 0 0 0
317993
(0.46 secs, 163,195,568 bytes)
ghci> input 0 0 1 0
9227647
(16.28 secs, 4,739,533,776 bytes)
```

圧倒的に高速、にはなった。

# 逆アセンブル

結局 `input.txt` は何を計算しているのか、読んでみよう。

```
01: cpy 1 a  ; a ← 1
02: cpy 1 b  ; b ← 1
;
03: cpy 26 d ; d ← 26
04: jnz c 2  ; if c /= 0 goto 06 ; part2
05: jnz 1 5  ;(if c == 0)goto 10 ; part1
; part2のみ
06: cpy 7 c  ; c ← 7
07: inc d    ; d++               ;
08: dec c    ; c--               ;
09: jnz c -2 ; if c /= 0 goto 07 ; d ← d + 7 ; (c ← 0)
; part1合流 つまり part1 のとき d = 26 part2 のとき d = 33
;
10: cpy a c  ; c ← a            ; save a to c
11: inc a    ; a++               ;
12: dec b    ; b--               ;
13: jnz b -2 ; if b /= 0 goto 11 ; a ← a + b; (b ← 0)
14: cpy c b  ; b ← c            ; restore a (in c) to b
15: dec d    ; d--
16: jnz d -6 ; if d /= 0 goto 10 ; while (d--) { (b,a) = (a, a+b); }
;
17: cpy 13 c ; c ← 13
18: cpy 14 d ; d ← 14                                 ;
19: inc a    ; a++               ;                     ;
20: dec d    ; d--               ;                     ;
21: jnz d -2 ; if d /= 0 goto 19 ; a ← a + 14; d ← 0 ;
22: dec c    ; c--                                     ;
23: jnz c -5 ; if c /= 0 goto 18                       ; a ← a + 14 * 13
```

つまり…

```c
d = 26 or 33

a = 1
b = 1
while (d--) { (b,a) = (a,a+b); }

a += 14 * 13;
```

26または33のフィボナッチ数を計算し、目くらましに \\(14 \times 13\\) を足している。

```haskell
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

part1c = fibs !! succ 26 + 14 * 13

part2c = fibs !! succ 33 + 14 * 13
```
