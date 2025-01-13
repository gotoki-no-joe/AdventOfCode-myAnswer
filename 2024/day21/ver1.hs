{-
頑張って早く解いて腕前を自慢するぞ！というより、めんどくせぇ～となるなこの問題。

ドアのボタンを押す系列は、入力で与えられている。
基本的に数字*Aという構成、というか凄い小さい。びっくり。

アームの先が X にあり、次に Y を押したいとき、こちらのパッドで操作するべき操作の系列、は手で作ろう。
と思ったけど面倒だぁ。
part1は、blankがどうとか言わないから、座標のマンハッタン距離で済むからそれで。

いや無理。シーケンスを作るために押すボタンの列を明確にしないとだから。
マジか…
-}

import Data.Char
import Data.Maybe

import Data.List
import qualified Data.Map as M
-- import Data.Array

-- キーのボタン、上下左右の順で行ける先
keylinks1, keylinks2 :: [(Char, String)]
keylinks1 =
  [('0',"2**A"),('A',"3*0*"),('1',"4**2"),('2',"5013"),('3',"6A2*"),('4',"71*5")
  ,('5',"8246"),('6',"935*"),('7',"*4*8"),('8',"*579"),('9',"*68*")]
keylinks2 =
  [('^',"*v*A"),('A',"*>^*"),('<',"***v"),('v',"^*<>"),('>',"A*v*")]

-- keylinksを使って、現在位置から押したいボタンに行くための系列の**一つ**を作って持っておく
keyseq1, keyseq2, keyseq1a, keyseq2a :: M.Map (Char, Char) String
keyseq1 = keyseqGen keylinks1
keyseq2 = keyseqGen keylinks2

keyseqGen :: [(Char, String)] -> M.Map (Char, Char) String
keyseqGen keylinks = loop seq0
  where
    keys = map fst keylinks
    seq0 = M.fromList [((k,k),"") | k <- keys] -- 同じキーに達する列はε
    s2 = length keys ^ 2
    loop m
      | M.size m == s2 = m -- 全部できた
      | otherwise = loop $ M.union m $ M.fromList
        [ ((a,e), f ++ [c])
        | (a,bs) <- keylinks -- aから上下左右に進むとbに行ける
        , (b,c) <- zip bs "^v<>" -- bに進むための1キー
        , b /= '*' -- 禁止方向
        , ((d,e),f) <- M.assocs m -- dからeに進むにはfをする
        , b == d -- 繋げたい
        , M.notMember (a,e) m -- 未知の組み合わせ
        ]

-- 目視で修正した版
keyseq1a = M.fromList
  [(('0','0'),""),(('0','1'),"<^"),(('0','2'),"^"),(('0','3'),"^>"),(('0','4'),"^^<"),(('0','5'),"^^")
  ,(('0','6'),"^^>"),(('0','7'),"^^<^"),(('0','8'),"^^^"),(('0','9'),"^^^>"),(('0','A'),">")
  ,(('1','0'),"v>"),(('1','1'),""),(('1','2'),">"),(('1','3'),">>"),(('1','4'),"^"),(('1','5'),"^>")
  ,(('1','6'),"^>>"),(('1','7'),"^^"),(('1','8'),"^^>"),(('1','9'),"^^>>"),(('1','A'),"v>>")
  ,(('2','0'),"v"),(('2','1'),"<"),(('2','2'),""),(('2','3'),">"),(('2','4'),"^<"),(('2','5'),"^")
  ,(('2','6'),"^>"),(('2','7'),"^^<"),(('2','8'),"^^"),(('2','9'),"^^>"),(('2','A'),"v>")
  ,(('3','0'),"v<"),(('3','1'),"<<"),(('3','2'),"<"),(('3','3'),""),(('3','4'),"^<<"),(('3','5'),"^<")
  ,(('3','6'),"^"),(('3','7'),"^^<<"),(('3','8'),"^^<"),(('3','9'),"^^"),(('3','A'),"v")
  ,(('4','0'),"vv>"),(('4','1'),"v"),(('4','2'),"v>"),(('4','3'),"v>>"),(('4','4'),""),(('4','5'),">")
  ,(('4','6'),">>"),(('4','7'),"^"),(('4','8'),"^>"),(('4','9'),"^>>"),(('4','A'),"vv>>")
  ,(('5','0'),"vv"),(('5','1'),"v<"),(('5','2'),"v"),(('5','3'),"v>"),(('5','4'),"<"),(('5','5'),"")
  ,(('5','6'),">"),(('5','7'),"^<"),(('5','8'),"^"),(('5','9'),"^>"),(('5','A'),"vv>")
  ,(('6','0'),"vv<"),(('6','1'),"v<<"),(('6','2'),"v<"),(('6','3'),"v"),(('6','4'),"<<"),(('6','5'),"<")
  ,(('6','6'),""),(('6','7'),"^<<"),(('6','8'),"^<"),(('6','9'),"^"),(('6','A'),"vv")
  ,(('7','0'),"vvv>"),(('7','1'),"vv"),(('7','2'),"vv>"),(('7','3'),"vv>>"),(('7','4'),"v"),(('7','5'),"v>")
  ,(('7','6'),"v>>"),(('7','7'),""),(('7','8'),">"),(('7','9'),">>"),(('7','A'),"vvv>>")
  ,(('8','0'),"vvv"),(('8','1'),"vv<"),(('8','2'),"vv"),(('8','3'),"vv>"),(('8','4'),"v<"),(('8','5'),"v")
  ,(('8','6'),"v>"),(('8','7'),"<"),(('8','8'),""),(('8','9'),">"),(('8','A'),"vvv>")
  ,(('9','0'),"vvv<"),(('9','1'),"vv<<"),(('9','2'),"vv<"),(('9','3'),"vv"),(('9','4'),"v<<"),(('9','5'),"v<")
  ,(('9','6'),"v"),(('9','7'),"<<"),(('9','8'),"<"),(('9','9'),""),(('9','A'),"vvv")
  ,(('A','0'),"<"),(('A','1'),"^<<"),(('A','2'),"^<"),(('A','3'),"^"),(('A','4'),"^^<<"),(('A','5'),"^^<")
  ,(('A','6'),"^^"),(('A','7'),"^^^<<"),(('A','8'),"^^^<"),(('A','9'),"^^^"),(('A','A'),"")]
keyseq2a = M.fromList
  [(('<','<'),""),(('<','>'),">>"),(('<','A'),">>^"),(('<','^'),"^>"),(('<','v'),">")
  ,(('>','<'),"<<"),(('>','>'),""),(('>','A'),"^"),(('>','^'),"^<"),(('>','v'),"<")
  ,(('A','<'),"v<<"),(('A','>'),"v"),(('A','A'),""),(('A','^'),"<"),(('A','v'),"v<")
  ,(('^','<'),"<v"),(('^','>'),"v>"),(('^','A'),">"),(('^','^'),""),(('^','v'),"v")
  ,(('v','<'),"<"),(('v','>'),">"),(('v','A'),"^>"),(('v','^'),"^"),(('v','v'),"")]

-- 全てのロボットのアーム位置をキーにしたDP的な配列をつくることになるのかな？

-- 一つ先のロボについて、
-- Aにいる状態から始めて、文字列seq（最後はA、条件ではないが）を入力するためのリモート側の操作列

remote1 keyseq code = concat $ zipWith step ('A':code) code
  where
    step s t = keyseq M.! (s,t) ++ "A"

-- 例：ロボ1に029Aと打たせるためにロボ2を操作する系列
test1 = remote1 keyseq1a "029A"
-- 例：という列をロボ2に打たせるためにロボ3を操作する系列
test2 = remote1 keyseq2a test1
-- 例：という列をロボ3に打たせるためにロボ4を操作する系列
test3 = remote1 keyseq2a test2

-- ひとつのコードに対する複雑さは、入力系列の長さと数字部分（前3文字）の数値の積
complexity code = (l, n, l * n)
  where
    l = length . remote1 keyseq2a . remote1 keyseq2a . remote1 keyseq1a $ code
    n = read $ init code

test4 = complexity "029A"

{-
やられた！
先頭のロボにとって ^>>, >^>, >>^ のコストは全て同じだけど、
次のロボにとっては違うんだ！

っていうて、A押したあと^>があるか<Vは遠くにあるので、
>^> みたいな系列が混じった、というだけで、フルスペックでDPしないとわからない、
ということではない。ので、手で修正した。
-}

-- run1 i = readFile i >>= print . sum . map complexity . lines

{-
まだ誰かダメっぽい。
ghci> complexity "029A"
(68,29,1972)
ghci> complexity "980A"
(60,980,58800)
ghci> complexity "179A"
(68,179,12172)
ghci> complexity "456A"
(64,456,29184)
ghci> complexity "379A"
(68,379,25772)

               1         2         3         4         5         6
      123456789o123456789o123456789o123456789o123456789o123456789o123456789o
029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
         <   A > A  v <<
最後のだ。
ghci> remote1 keyseq2a . remote1 keyseq2a . remote1 keyseq1a $ "379A"
     "v<<A>>^AvA^Av<<A>>^AAv<<A>A^>AAvAA^<A>Av<A^>AA<A>Av<A<A>>^AAA<Av>A^A"
         <   A > A   <   A    < V  AA >>

ダメなんだ。じゃあマジどうしたらいいのと。
縦と横と両方使うとき、可能性があるときは両方とも試す、間に挟むのは確実にナシとしても、
という感じか？

リストモナドをここにすんなり差し込める？

そうでないとすると、ガチでDPだな。
因果関係を逆にイメージして、
・テンキーの方でaからbに移動して押したい、という要求を最短で達成する列をそれぞれ作りたい。
といっても、テンキーの前にいるロボットがアームを動かした、ボタンを押した、瞬間というのは、
それらを操作している方の指は必ずAにあるはずで…ではないな。
最初のカーソルキーを操作しているロボは、カーソルを押した瞬間に腕が動くから、
その状態を考慮する必要があるということだ。

なんかもう幅優先探索でやった方が早くね？
入力は結局^v<>Aの5通りで、どこかの層でドボンしたらそれは枝刈り、一番奥が違うボタンを押したら枝刈り、で行けないか？
千日手を消すために、探査済みの状態をどうやって見つけるかがわからない。

4本の指が今どこを指しているか、が 11 x 5^3 = 1375 とおり
4台のオートマトンの直列つなぎ

AAAAの状態から開始して、一番奥のオートマトンがコード4文字を出力する最短の入力系列を求めたい
文字を出力するとき、AM1は任意の位置に居られる、その文字が出る

オートマトンとして捉えるとき、どこが状態だ？ロボか？

テンキーに面しているロボ1 : 状態11とおり、入力文字5とおり、出力文字11とおり 0-9A
ロボ1を操作しているロボ2 : 状態5とおり、入力文字5とおり、出力文字5とおり <^v>A
ロボ2を操作しているロボ3 : 状態5とおり、入力文字5とおり、出力文字5とおり <^v>A
ロボ3を操作している俺

全体で見ると、状態11*5*5=275のロボ、入力文字5とおり、出力文字11とおり、となる。

全ての状態の組み合わせから、次にxxを出力する最短のやり方、が知りたいように見える。
ロボ1が何かの文字を送出するのは、Aが入ったとき
ロボ1にAが入るのは、ロボ2の状態がAでAが入ったとき
ロボ2にAが入るのは、ロボ3の状態がAでAが入ったとき
ロボ3にAが入るのは、俺がAを押したとき
なので、xxを出力した瞬間、はロボ1はxx、ロボ2,3は必ずAにいる。

ということは、現在 0～9A/A/A の状態からスタートして、次に 0～9A/A/A に至る最短系列をそれぞれ見つけたいだけ。
11x11=121通り。
そうとわかれば、幅優先探索で訪問済みの状態を把握して調査できるな。

オートマトンの状態遷移をどう書けばいいのやら。

状態遷移表からやるしかないんか。

ロボ1は、状態11個 0～10
入力文字は、無入力(0)の他に5とおり ^v<>A
出力文字は 0～9Aの11個と、出力なしもある

無入力のときは出力なし遷移なし、はデフォルトでそうだろう。
各状態でAが入ったとき、文字を出力して同じ状態に戻る
tr(q,'A')='A', out(q,'A') = f(q)
各状態で^v<>が入ったとき、エラーになるか、出力なしで状態遷移する
-}

type Automata a = a -> Char -> Maybe (a, Maybe Char)

robo1 :: Automata Char
--robo1 :: Char -- 状態
--      -> Char -- 入力
--      -> Maybe (Char, Maybe Char) -- 遷移先と出力
robo1 st 'A' = Just (st, Just $ "0123456789A" !! robo1code st)
robo1 st  c  =
  case c of
    '<' -> amsub "**12*45*780" $ robo1code st
    '>' -> amsub "A23*56*89**" $ robo1code st
    '^' -> amsub "2456789***3" $ robo1code st
    'v' -> amsub "**0A123456*" $ robo1code st

robo1code 'A' = 10
robo1code  c  = digitToInt c

amsub tbl st =
  case tbl !! st of
    '*' -> Nothing
    c   -> Just ( c , Nothing)

robo2 :: Automata Char
robo2 st 'A' = Just (st, Just $ "^v<>A" !! robo2code st)
robo2 st  c  =
  case c of
    '<' -> amsub "*<*v^" $ robo2code st
    '>' -> amsub "A>v**" $ robo2code st
    '^' -> amsub "*^*A*" $ robo2code st
    'v' -> amsub "v***>" $ robo2code st

robo2code '^' = 0
robo2code 'v' = 1
robo2code '<' = 2
robo2code '>' = 3
robo2code 'A' = 4

-- 合体ロボ

{-
robo3 :: AUtomata
robo123 (st3,st2,st1) c = undefined
  where
    case robo2 st3 c of
      Nothing -> Nothing
      Just (st3a, Nothing) -> Just ((st3a, st2, st1), Nothing)
      Just (st3a, Just d) ->
        case robo2 st2 d of
          Nothing -> Nothing
          Just (st2a, Nothing) -> Just ((st3a, st2a, st1), Nothing)
          Just (st2a, Just e) ->
            case robo1 st1 e of
              Nothing -> Nothing
-}

gattai :: Automata a -> Automata b -> Automata (a,b)
gattai roboA roboB (stA, stB) c =
  do
    (stA1, mc) <- roboA stA c
    case mc of
      Nothing -> return ((stA1, stB), Nothing)
      Just d -> do
        (stB1, me) <- roboB stB d
        return ((stA1, stB1), me)

robo321 = gattai robo2 $ gattai robo2 robo1

-- 入力系列を喰わせて、出力を観察しよう

runAutomata am str = (st, catMaybes res, res)
  where
    (st, res) = mapAccumL step (Just ('A',('A','A'))) str
    step Nothing _ = (Nothing, Nothing)
    step (Just st) c =
      case am st c of
        Nothing -> (Nothing, Nothing)
        Just (st, md) -> (st, md)

test1 = runAutomata robo321 "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" -- 029A
test2 = runAutomata robo321 "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" -- 980A
test3 = runAutomata robo321 "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 179A
test4 = runAutomata robo321 "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" -- 456A
test5 = runAutomata robo321 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 379A

