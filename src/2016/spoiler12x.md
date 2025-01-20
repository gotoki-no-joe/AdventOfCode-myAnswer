# 設計

汎用レジスタが4つと、命令ポインタ（プログラムカウンタ）がある小さなプロセッサをエミュレーションする。

レジスタ名が `a`～`d` なので、そういう添え字の配列でプロセッサの状態を表現する。
さらにプログラムカウンタもあるので、`pred 'a'` をそれにあてる。

```haskell
import Data.Array.Unboxed

ip = pred 'a'

type State = UArray Char Int -- 添え字の範囲は (ip, 'd')
```

プロセッサの動作は、どの命令でも同じ、命令フェッチなどの部分と、
取り込んだ命令に応じて固有の動作をする部分に分けられる。
固有の部分は、命令ポインタを含むレジスタの内容を変更する関数で表せる。
つまり次のような型を持つ関数が「命令」である。

```haskell
type Instr = State -> State
```

プログラムは命令の配列として表現し、
現在の命令ポインタがプログラムを逸脱していないならそこにある命令を取り出して実行、
さもなくば停止する、というハンドラがエミュレータ本体となる。

```haskell
run :: Array Int Instr -> [Int] -> UArray Char Int
run prog xs = loop v0
  where
    v0 = listArray (ip, 'd') $ fst (bounds prog) : xs
    loop v
      | inRange (bounds prog) (v ! ip) = loop $ prog ! (v ! ip) $ v
      | otherwise = v
```

実際の命令は、入力されるプログラムに応じて `Instr` 型の関数を生成する。
例を示すと、

- `cpy 41 a` : `\v -> accum (flip ($)) v [(ip, succ), ('a', const 41)]`
- `cpy c d`  : `\v -> accum (flip ($)) v [(ip, succ), ('a', const (v ! 'c'))]`
- `inc b`    : `\v -> accum (flip ($)) v [(ip, succ), ('b', succ)]`
- `jnz a 2`  : `\v -> accum (flip ($)) v [(ip, if v ! 'a' == 0 then succ else (2 +))]`

入力の文字列に対して、このような関数を生成する「パーサ」は、実体としてアセンブラと言える。

```haskell
parse :: String -> Instr
parse l =
  case words l of
    ['c':_, x, y:_] | isReg x -> \v -> accum (flip ($)) v [(ip, succ), (y, const (v ! head x))]
                  | otherwise -> \v -> accum (flip ($)) v [(ip, succ), (y, const (read x))]
    ['i':_, x:_]              -> \v -> accum (flip ($)) v [(ip, succ), (x, succ)]
    ['d':_, x:_]              -> \v -> accum (flip ($)) v [(ip, succ), (x, pred)]
    ['j':_, x, y]   | isReg x -> \v -> accum (flip ($)) v [(ip, if v ! head x == 0 then succ else (read y +))]
                  | otherwise -> \v -> accum (flip ($)) v [(ip, if read x     == 0 then succ else (read y +))]

isReg (c:cs) = elem c "abcd" && null cs
```

# いつもの

## 入力

命令の配列としてのプログラムを生成して渡す。

```haskell
runner i f = do
  is <- map parse . lines <$> readFile i
  let prog = listArray (1, length is) is
  print $ f prog
```

## パート1

全てのレジスタを0で開始する。

```haskell
part1 prog = run prog [0,0,0,0] ! 'a'

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1
```

## パート2

`c` レジスタを1で開始する。

```haskell
part2 prog = run prog [0,0,1,0] ! 'a'

main2 = runner "input.txt" part2
```

# さらに

命令を単体で考えると、命令ポインタを1増やしたりする仕事と、次の命令の実行に移る仕事は別になるが、
ここでは、それぞれの命令がどのアドレスに格納されているのかも決まっている。

<!--
読んだら？

cpy 1 a  ; a <- 1
cpy 1 b  ; b <- 1
cpy 26 d ; d <- 26
jnz c 2  ; if c == 0 goto Label1 -- part1/2 switch
jnz 1 5  ; --
cpy 7 c  ; c <- 7
         ; do
inc d    ;   d++
dec c    ;   c--
jnz c -2 ; while c /= 0 ==> d += 7; (c <- 0 cancel) ==> part1ならd=26,part2ならd=33

Label1:
         ; do
cpy a c  ;   c <- a
         ;   do
inc a    ;     a++
dec b    ;     b--
jnz b -2 ;   while b /= 0 ==> a += b; (b <- 0 cancel)
cpy c b  ;   b <- c
dec d    ;   d--
jnz d -6 ; while d /= 0 ==> 26 or 33回 これFibでは？

cpy 13 c ; c <- 13
         ; do
cpy 14 d ;   d <- 14
         ;   do
inc a    ;     a++
dec d    ;     d--
jnz d -2 ;   while d /= 0 ==> a += 14; d <- 0
dec c    ;   c--
jnz c -5 ; while c /= 0 ==> a += 13 * 14

-->
