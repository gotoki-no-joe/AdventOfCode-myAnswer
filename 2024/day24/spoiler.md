# 入力

`input.txt` まで覗いて信号名は3文字であることを確認し、その知識を使うと、
前半は前3文字と、末尾の1文字(0または1)が`1`かどうかの`Bool`値のペアにできる。

```haskell
parse1 :: String -> (String, Bool)
parse1 l = (take 3 l, last l == '1')
```

後半は、`words`で切り分けて矢印以外をタプルに詰めておけばとりあえずはよいかと。

```haskell
parse2 :: String -> (String, String, String, String)
parse2 l = (a,b,c,d)
  where
    a:b:c:_:d:_ = words l
```

そんな要素のリストふたつを渡す。

```haskell
runner i f = do
  (ls1, _:ls2) <- break null . lines <$> readFile i
  let p1 = map parse1 ls1
  let p2 = map parse2 ls2
  print $ f p1 p2

test1 = runner "samp1.txt" part1
test2 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

part1 p1 p2 = ...
```

# パート1

過去のAoCでは、似たような話で、その上ループで信号が流れ続ける難問（というか解けていない）があったが、
これは信号の状態は一度確定したらそのままの静的なシステムなのでごく簡単。

信号名からその状態を取り出せる `Map String Bool` を作り、

- 入力信号はその値を普通に設定
- ゲート出力は、そのマップを参照して入力を取り出し、結果出力を設定

という循環参照を遅延評価で解決させるいつものやつで、計算はあっという間にできてしまう。

```haskell
import qualified Data.Map as M

part1 p1 p2 = ...
  where
    m = M.fromList $ p1 ++ [(d, op b (m M.! a) (m M.! c)) | (a,b,c,d) <- p2]
    op "OR" = (||)
    op "AND" = (&&)
    op "XOR" = (/=)
```

Haskell使えない人が見たら「ズルイ！」と叫びそうな解答プログラムだ。

後は、このマップからあるだけ `z` な信号を取り出して、二進数を読み取るだけ。

```haskell
part1 p1 p2 =
    sum $                                                     -- 足し合わせる
    zipWith (\i b -> if b then i else 0) (iterate (2 *) 1) $ -- Trueのとき2^kを
    map (m M.!) $ takeWhile (flip M.member m)         -- マップにある間だけ取り出し、マップを読み、
    ['z' : tail (show i) | i <- [100 ..]]             -- 信号名を無限に生成
```

<!--
## 先行評価だと？

Haskellどっぷりなせいで、こういうことができない言語だとどうやって計算するのかピンとこなくなってきた。

- マップ `gate` :ゲートは接続先の出力信号で一意に区別できる。ゲートの演算と入力信号名を書いておく。
- マップ `obs` :信号名をキーに、その信号を入力で取り込んでいるゲートの一覧を持つマップも作っておく。
- マップ `sig` :信号の状態を記録する。最初は空。
- パズル入力前半を使って、入力信号の内容を `sig` に割り当てていく。
  - `obs`を使って、その信号を見ているゲートを順にチェックする。
  - 入力が両方揃ったのなら、その出力も次に `sig` に割り当てるキューに入れる（その場で再帰計算してもいけるのかも）
- 全ての入力信号を割り当て、ゲート出力の波及が全て止まったら、全ての信号の状態が確定している

という感じか。
再帰呼び出しのスタックが許せば、メモ化再帰のスタイルでも計算できるか。
-->

# パート2

なんだかとっても厄介そうな話になってきた。
`input.txt`の行で数えてゲート、つまり入れ替え候補は 313-91 = 222個
総当たりで試すとすると場合の数は ${}_{222} C_8 = 128,795,283,347,445$ 通り。

もう少し特徴を調べてみると、入力信号は 45ビットのxとy、出力信号zは46ビット。
総当たりの総当たりは気が遠くなりすぎる。

ところで、意地悪な回路になっているのだろうか。

二進数1桁ぶんの、最も単純な加算器は、XとYの2ビットを、和Sと上の桁へのくり上がりCoutに変換する。
これは
```
X xor Y = S
X and Y = Cout
```
と作れる。これをハーフアダーという。
加算がこれで済むのは1の位の桁だけで、それより上は、下の桁からのくり上がりを含めた3ビットを、SとCに変換する回路になる。
これは、ハーフアダー2つとORゲート1つで作れる。
```
X xor Y = T -- halfadder 1
X and Y = U

T xor Cin = S -- halfadder 2
T and Cin = V

U or  V = Cout
```
T,U,Vは内部信号。
これをフルアダーという。
ただし、X,Y,Cinのどれを先に足すかは自由なので、回路の構造はバリエーションが存在しうる。

さて、入力が45桁のとき、ハーフアダー1つ、フルアダー44個が必要。
使うゲートの個数は $2 + 5 \times 44 = 222$ ぴったり一致。つまりこの回路構成に対して、一切の無駄がない。
ということで、この構造から外れている箇所を分析すればよい。
エディタで開いてにらめっこでは能が無いので、必要な情報を取り出して補助するプログラムを作っていこう。

## 道具1

最も基本的な道具として、信号名2つを指定して、その二つを直接使っているゲートを全て挙げる関数を作る。
信号名は辞書順にしたマップに情報を入れておいて検索すればよいだろう。

応用として、それを使って `x00`, `y00` からどのように接続がされているかを調べてみる。

```haskell
main2 = runner "input.txt" part2

part2 _ p2 = graph "x00" "y00"
  where
    g = M.fromListWith (++) [((min a c, max a c), [(b, d)]) | (a,b,c,d) <- p2]
    graph a b = M.findWithDefault [] (min a b, max a b) g
```

```
ghci> main2
[("AND","hfm"),("XOR","z00")]
```

ちゃんとハーフアダーになっていて、和は `z00` にちゃんとつながっている。
ということは2の位へのくり上がりは `hfm` なのだろう。

## 道具2

以降のフルアダーの接続が正しいかどうかを追跡する関数を作る。
調査したいビット位置、下の位からのくり上がり信号の名前、を引数としてとり、
正しく接続がされているときは次の桁へのくり上がり信号の名前を、
何か異常があるときは `Nothing` を返すようにする。

先にハーフアダーをかける信号2つの組み合わせ3とおり全てを試すようにする必要があることに注意。

```haskell
import Control.Applicative
import Control.Monad

    check k cin = sub x y cin <|> sub y cin x <|> sub cin x y
      where
        x = 'x' : tail (show $ k + 100)
        y = 'y' : tail x
        z = 'z' : tail x
        sub a b c = do
          let gab = graph a b
          t <- lookup "XOR" gab
          u <- lookup "AND" gab
          let gtc = graph t c
          zc <- lookup "XOR" gtc
          guard $ zc == z
          v <- lookup "AND" gtc
          lookup "OR" $ graph u v
```

Maybeモナドでいい感じに書けた。

## 道具3:主戦力

これを2の位から順に調べて、異常を検知したところで停止し、状況を報告するプログラムにする。

```haskell
part2 _ p2 = checkloop 1 c00
  where
    Just c00 = lookup "AND" $ graph "x00" "y00"

    checkloop k cin =
      case check k cin of
        Just cout -> checkloop (succ k) cout
        Nothing   -> (k, cin, graph x y, graph y cin, graph cin x)
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x
```

動かす。

```
ghci> main2
(7,"btq",[("AND","jss"),("XOR","krv")],[],[])
```

## 手作業

`x07`, `y07`, `z07`, `btq`, `jss`, `krv` がどうなっているのか調べる。
これはもう `input.txt` をエディタで見るか `grep` する方が早い。
その結果を本来の接続と見比べると、 `nqk` と `z07` を入れ替えれば直るとわかる。

なのでそこを直す。
ただし、`input.txt` を編集してではなく、読み込んだデータの上だけ。

```haskell
--  g = M.fromListWith (++) [((min a c, max a c), [(b,      d)]) | (a,b,c,d) <- p2]
    g = M.fromListWith (++) [((min a c, max a c), [(b, repl d)]) | (a,b,c,d) <- p2]

    repl "nqk" = "z07"
    repl "z07" = "nqk"
    repl sig = sig
````

一箇所直したので、調査を再起動し、次の誤りを見つける。4回これをやると、

```
ghci> main2
(45,"z45",[],[],[])
```

となって、MSBを越えたオーバーフローのキャリーフラグまで接続に問題ないことが確認できた。

## 完成

4箇所で入れ替えた8つの信号名をソートして連結すれば、入力欄に入れる文字列が完成する。

```haskell
import Data.List

part2ans = intercalate "," $ sort
  [ "nqk", "z07"
  , ...
```

ということで、パート2はプログラミングよりも手で分析する時間の方が長かったし、
コンピュータアーキテクチャ（のごく基本的な部分だけど）の知識も必要な、
コンピュータサイエンスの広い知見を問われる、興味深い問題だったと思います。
