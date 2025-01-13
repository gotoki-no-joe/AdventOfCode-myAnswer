# 入力

例によって、数字文字以外を無視して整数列にし、
先頭3つをレジスタの初期値と解釈するようにして、全体をそのまま渡してしまおう。

```haskell
import Data.List.Split
import Data.Char

runner i f = do
  xs <- map read . wordsBy (not . isDigit) <$> readFile i
  print $ f xs

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int]
part1 (rA:rB:rC:prog) = ...
```

# パート1

とにかくこのプロセッサのエミュレータを実装しないと始まらない。

ひとつの命令に関する処理の形を、
命令ポインタを含めたプロセッサの状態を変化させる関数だと考えると

```haskell
doInstruction :: State -> State
```

`out` 命令の結果の行き先がなくなる。
なので、このような型の関数を命令コードに応じてディスパッチして、
戻ってきたらまた次の命令をディスパッチして、という流れにはしにくい。

ディスパッチする関数は個々の処理を起動して、
個々の処理は完了後に行進した状態を呼び出し元に戻す代わりに、
次の命令に進むためにディスパッチ関数を呼び出す、という末尾呼び出し構造にしよう。

Stateモナドとか使いたくなるが、それをすると出力を Writer モナドに捕まえられて、
lazyに途中結果を覗くみたいなやり方ができなくなるので我慢する。

プログラムは大した長さではないので、配列に入れずに `(!!)` でアクセスしてしまう。

```haskell
data State = State {iP :: Int, regA :: Int, regB :: Int, regC :: Int}

runCPU :: [Int]  -- レジスタ初期値＋プログラム、入力データ
       -> [Int]  -- out出力の系列
runCPU (rA0:rB0:rC0:prog) = exec state0
  where
    ub = pred $ length prog -- アドレス上限
    state0 = State {iP = 0, regA = rA0, regB = rB0, regC = rC0}

    exec :: State -> [Int]
    exec st
      | pc < 0    = error "negative iP"
      | pc == ub  = error "cannot load operand"
      | ub < pc   = [] -- halt
      | otherwise = (instr !! (prog !! pc)) st
      where
        pc = iP st

    instr :: [State -> [Int]]
    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]
```

## オペランドロード

オペランドの読み込みが2とおりある。
どちらも `State` を引数とする（そして暗黙に `prog` を読む）関数として定義しておく。

```haskell
    literal st = prog !! succ (iP st)

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x
```

## 個々の命令の実行

個々の命令は終了後、各自で `iP` を2増やし、`exec` を末尾呼び出しする。

### adv, bdv, cdv

これは割り算というより「Aレジスタの内容をコンボオペランドだけ右シフトし、Aレジスタに書き戻す」という動作をする。
後ろ二つも、格納先は違えど、被除数はA。

```haskell
import Data.Bits

    adv st = exec $ st {iP = 2 + iP st, regA = shiftR (regA st) (combo st)}
    bdv st = exec $ st {iP = 2 + iP st, regB = shiftR (regA st) (combo st)}
    cdv st = exec $ st {iP = 2 + iP st, regC = shiftR (regA st) (combo st)}
```

### jnz

これのために、各命令が自分で `iP+2` をするはめになっている。

```haskell
    jnz st
      | regA st == 0 = exec $ st {iP = 2 + iP st}
      | otherwise    = exec $ st {iP = literal st}
```

### out

出力はHaskellの普通のリスト関数のように行う。

```haskell
    out st = 7 .&. combo st : exec (st {iP = 2 + iP st})
```

### bxl, bst, bxc

書いてあるとおり。

```haskell
    bxl st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (literal st)}
    bst st = exec $ st {iP = 2 + iP st, regB = 7 .&. combo st}
    bxc st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (regC st)}
```

## 完成

`runCPU` がパート1そのものになってしまった。

```haskell
part1 :: [Int] -> [Int]
part1 = runCPU
```

# パート2

クワイン(Quine)かぁ。

適当にAレジスタの値を差し替えて `runCPU` を起動し、出てきた結果を元のプログラムと比較する、
というプログラムにおいて、正格言語だと `runCPU` の終了を待つことになるが、
Haskellは遅延評価するので、最初に違いが生じた時点で破棄できる。
なのでブン回せばヨシ！

ただし、パート1では、バグがあったときにわかるように各所に `error` を仕掛けたが、
レジスタAの初期値の問題で異常事態に陥ったとき、これがブン回しが途中でコケるのはかなわないので、
少し改造する。

分岐は `jnz` 命令で起きて、即値オペランドなので、プログラムに変なことが書いてない限り、
変なところに飛ぶ心配はない。なので `exec` の二つの `error` は、場合分けごと消して問題ない。

```haskell
    exec :: State -> [Int]
    exec st
--      | pc < 0    = error "negative iP"
--      | pc == ub  = error "cannot load operand"
      | ub < pc   = [] -- halt
      | otherwise = (instr !! (prog !! pc)) st
      where
        pc = iP st
```

コンボオペランドのアドレッシングモード7も、パート1で遭遇しなかったから発生することはなく、
そのまま放置で構わない。
パラノイアな人は、コンボオペランドを使う命令の実行前に、オペランドをのぞき見し、
それが7なときは 0～7 でない値を出力して完了するコードを仕込んでもいい。

ブン回す準備ができた。
状況を見守るため、時々実行状況を表示するように仕掛けよう。

```haskell
import Debug.Trace

test2 = runner "samp2.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> Int
part2 (_:xs) = head
  [ a
  | a <- [0 ..], mod a 10000 /= 0 || traceShow a True
  , runCPU (a : xs) == drop 2 xs]
```

```
ghci> test2
0
10000
20000
30000
40000
50000
60000
70000
80000
90000
100000
110000
117440
```

ではこれで本番の方も、時間かかりそうなのでコンパイルして実行すれば何とか…

…なるわけない。  
これは辛い戦いになりそうだ。

## パート2のサンプルを読む。

```
0 : 0,3 ; A ← A >> 3
1 : 5,4 ; out A
2 : 3,0 ; jnz 0
```

つまり、Aの下位3ビットを順に出力していき、最後に0を出力したらそこで完了する。
このプログラムに 0,3,5,4,3,0 という出力系列を出させるための A の初期値は
最初の3ビットシフトで使われずに捨てられる値も含めて

```haskell
ghci> foldr (\x acc -> acc * 8 + x) 0 [0,0,3,5,4,3,0]
117440
```

と求められる。

## 自分のパズル入力を分析する

詳細は省くが、ハンド逆アセンブルして読み解くと、

- Aレジスタの値が3ビットずつ消費されていき、最後に0になったら終わる、サンプルと同じ構造
- 毎回のループの中で、出力データを生成するためにB,Cレジスタは初期化されてから使われ、ループ間で引き継ぎはない

ということがわかる。このプログラムがループを回りながら実行が進む様子を逆再生したところをイメージすると、

- 最後の出力のとき、Aレジスタは下位3ビットより他は0になっているはずなので、それだけから最後の出力は決まる
このときのAの値を $A_0$ とする。これは8回試せば見つかる。
- その一つ前の出力を計算した後、Aレジスタは $A_0$ になる。
その前は $0 \leq Y_0 \leq 7$ な何らかの値で $A_1 = A_0 * 8 + Y_0$ と表される6ビットの値であったはずで、
$Y_0$ がラスト前の値を決定する。これは $8 \times 8 = 64$ とおりを試す必要はなく、
$A_0$ が既知ならば8通り試すだけで見つかる。
- その一つ前の出力を計算した後、…

と、順に推理していけると気がつく。つまりこのプログラムの性質として、

「Aレジスタの初期値 $X$ が、実行によりある系列 $O$ を出力するならば、
$X \times 8 + Y$ $(0 \leq Y \leq 7)$ は、一つ値を出力して、その後 $O$ を出力する。」

が成り立っていると推理できる。

ということでこれを（手で実行するのでなく）プログラムにやらせよう。

## 解析プログラム

プログラムの `tails` を短い方から順に調べる。
長さ 0 の正しい出力をするための A の仮想的な初期値 0 から始めて、
プログラムの末尾をもう一つ手前から出力するような A の初期値は、
前の初期値×8+(0～7) のいずれかである。

```haskell
import Data.List

part2 :: [Int] -> [Int]
part2 (_:xs) = ans
  where
    prog = drop 2 xs
    ans = foldr step [0] $ init $ tails prog
    step progtail cands = -- traceShowId を差し込むと様子を見られる
      [ a
      | c <- cands, d <- [0 .. 7], let a = c * 8 + d
      , runCPU (a:xs) == progtail]
```
