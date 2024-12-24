{-
信号名3文字、コロン、0か1
と
3文字、OR/AND/XOR/ 3文字、矢印、3文字
か。
-}

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Applicative

runner i f = do
  (ls1, _:ls2) <- break null . lines <$> readFile i
  let p1 = map parse1 ls1
  let p2 = map parse2 ls2
  print $ f p1 p2

parse1 :: String -> (String, Bool)
parse1 l = (take 3 l, last l == '1')

parse2 :: String -> (String, String, String, String)
parse2 l = (a,b,c,d)
  where
    a:b:c:_:d:_ = words l

test1 = runner "samp1.txt" part1
test2 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

part1 p1 p2 = ans
  where
    m = M.union m1 m2
    m1 = M.fromList p1
    m2 = M.fromList [ (d, op b (m M.! a) (m M.! c)) | (a,b,c,d) <- p2]
    op "OR" = (||)
    op "AND" = (&&)
    op "XOR" = (/=)

    ans = loop 0 1 100
    loop acc bit k
      | M.notMember key m = acc
      | m M.! key = loop (acc + bit) (bit + bit) (succ k) -- 引数の順序が失敗だ
      | otherwise = loop acc         (bit + bit) (succ k)
      where
        key = 'z' : drop 1 (show k)

{-
パート2
前半は関係なかったと。
313 - 91 = 222 の要素がある。
総当たりの場合の数は
222 * 221 / 2 * 220 * 219 / 2 * 218 * 217 / 2 * 216 * 215 / 2 = 649,128,228,071,122,800
bruite-forceは無理、と。

half-adderは、xorでSが作れる。Cは、andで作れる。
full-adderは、half-adderを二段に重ねて、carryをorすれば作れる
つまりxor *2, and *2, or *1 の5つ。

入力は45ビットあるようだ。44 * 5 + 2 = 222 なのでぴったりだ。
つまり遊びは全くなくてきっかりそういう構造になっているハズなので、
正しいグラフを作って、食い違っているところを見つけたら済む感じだな。

しかし、ハーフアダーの組み合わせがどうなるかはバリエーションがあるから、一意には決まらない。

Xnn XOR Ynn -> (S1)
(S1) XOR (Cin) -> Znn

だから組み合わせは3通りあるって。

なので00の桁から、グラフを辿ることで、Z00に繋がる経路を確認することと、
Coutに相当する、次の桁の一つの入力がどれかを特定する、という作業を
1桁ずつ進めていけばいいのかな。

Xnn --x +--+ co ------co2  O
        |HA|               R -- Cout
Ynn --y +--+ s---+--+ co1  2
                 |HA|
Cin --c ---------+--+ s -------- Znn


(in1 XOR in2) XOR in3 = Znn


HALF-ADDER

X00 XOR Y00 -> Z00
X00 AND Y00 -> C00

これで、C00がどれか確認できる。

FULL-ADDER

Xnn, Ynn, Cin を A,B,C として
A XOR B -> T
T XOR C -> Znn

A AND B -> U
T AND C -> V
U OR  V -> Cout

これを調べやすいデータ構造と調べるプログラムはどうすればいいかしら。

2つの入力信号に対して、それを使う演算と、その出力先を、あれば返す
graph :: String -> String -> Maybe (String, String)

3つの入力信号に対して、その3つのxorをとっている信号があれば、
中間に使っている信号Tとともに返す...Tいらん
xor3 :: String -> String -> String -> Maybe String

3つの入力信号に対して、そのうちxorをとっている2つの組についてandをとっていて、
xorの結果ともう一つについてandをとっていて、
そのふたつの結果をorする接続になっているかを探し、
そうなら最後の出力が何かを返す
cout :: String -> String -> String -> Maybe String

正しい接続かはこれでわかるけど、
正しくないときは、graphで手作業で探るか。

これがやりやすいのは、graphとしてMap (String,String) -> (String,String) をほぼそのまま使うことか。

演算もキーにしないといかんのか！？重なるもんな。

-}

main2 = runner "input.txt" part2

part20 _ p2 = checkloop 1 c00
  where
    g = M.fromListWith (++) [((min a c, max a c), [(b, d)]) | (a,b,c,d) <- p2]
    graph a b = M.findWithDefault [] (min a b, max a b) g

    Just c00 = lookup "AND" $ graph "x00" "y00"

    check1 k cin = check2 x y cin <|> check2 y cin x <|> check2 cin x y
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x
        z = 'z' : drop 1 x
        check2 a b c = do
          let gab = graph a b
          t <- lookup "XOR" gab
          u <- lookup "AND" gab
          let gtc = graph t c
          zc <- lookup "XOR" gtc
          guard $ zc == z
          v <- lookup "AND" gtc
          lookup "OR" $ graph u v

    checkloop k cin =
      case check1 k cin of
        Just cout -> checkloop (succ k) cout
        Nothing -> (k, cin, graph x y, graph y cin, graph cin x)
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x
-- 一旦放棄

{-
ghci> main2
(7,"btq")

x,y,cinの接続先を見る
ghci> main2
(7,"btq",[("AND","jss"),("XOR","krv")],[],[])

zの接続元も見ないとだね。
というか、落ち着いて考えたら、zにXORでないものが繋がっていたらその瞬間おかしいんだ。

-}

checkz _ p2 = [(d, b) | (_,b,_, 'z':d) <- p2, b /= "XOR"]

{-
ghci> runner "input.txt" checkz
[("45","OR"),("24","AND"),("32","AND"),("07","OR")]
これだけで、Zxxに間違って繋がっている4本の線が明らかになった。

ORをとるのはcarryのとき限定なので、そっちに繋げと。ただしそれがどれかわからない。

とりあえず、7のCINとして使うべき信号がz07に入っているっぽいので、
check1 7 "z07" してみるか…違った。

もうgrep使った方がはやいな。ないけど。

(7,"btq",[("AND","jss"),("XOR","krv")],[],[])

X07 XOR y07 -> krv # T
krv XOR btq -> nqk # Z07にする必要がある

x07 AND y07 -> jss # U
btq AND krv -> stq # V
stq OR jss -> z07 # Coutにする必要がある

つまり nqk と z07 を差し替える。

次。

ghci> main2
(17,"nmq",[("AND","fgt"),("XOR","pcp")],[],[])


x17 XOR y17 -> pcp # T
nmq XOR fgt -> z17 # fgtとpcpが違う？

y17 AND x17 -> fgt # U
fgt AND nmq -> vtv # V
vtv OR pcp -> fcg  # Cout

次。

ghci> main2
(24,"nmp",[("AND","z24"),("XOR","mqq")],[],[])

y24 XOR x24 -> mqq # T
nmp XOR mqq -> fpq # Z24でないといかん

x24 AND y24 -> z24 # Uになる信号でないといかん
nmp AND mqq -> rgc # V
rgc OR fpq -> tgs  # Cout

次。

ghci> main2
(32,"fth",[("AND","fqc"),("XOR","qcs")],[],[])

x32 XOR y32 -> qcs # T

x32 AND y32 -> fqc
fth XOR qcs -> srn # z32でないといかん

x32 AND y32 -> fqc # U
fth AND qcs -> z32 # V
srn OR fqc -> pcs # Cout


-}

part2 _ p2 = checkloop 1 c00
  where
    g = M.fromListWith (++) [((min a c, max a c), [(b, repl d)]) | (a,b,c,d) <- p2]
    repl "nqk" = "z07"
    repl "z07" = "nqk"
    repl "fgt" = "pcp"
    repl "pcp" = "fgt"
    repl "fpq" = "z24"
    repl "z24" = "fpq"
    repl "srn" = "z32"
    repl "z32" = "srn"
    repl sig = sig
    graph a b = M.findWithDefault [] (min a b, max a b) g

    Just c00 = lookup "AND" $ graph "x00" "y00"

    check1 k cin = check2 x y cin <|> check2 y cin x <|> check2 cin x y
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x
        z = 'z' : drop 1 x
        check2 a b c = do
          let gab = graph a b
          t <- lookup "XOR" gab
          u <- lookup "AND" gab
          let gtc = graph t c
          zc <- lookup "XOR" gtc
          guard $ zc == z
          v <- lookup "AND" gtc
          lookup "OR" $ graph u v

    checkloop k cin =
      case check1 k cin of
        Just cout -> checkloop (succ k) cout
        Nothing -> (k, cin, graph x y, graph y cin, graph cin x)
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x

{-
ghci> main2
(45,"z45",[],[],[])

はい、完成。
-}

part2ans = intercalate "," $ sort
  [ "nqk", "z07"
  , "fgt", "pcp"
  , "fpq", "z24"
  , "srn", "z32"
  ]
