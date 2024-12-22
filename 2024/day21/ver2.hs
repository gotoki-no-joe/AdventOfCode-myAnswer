import Data.Char
import Data.Maybe
import Data.List

import qualified Data.Map as M

import Debug.Trace

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

gattai :: Automata a -> Automata b -> Automata (a,b)
gattai roboA roboB (stA, stB) c =
  do
    (stA1, mc) <- roboA stA c
    case mc of
      Nothing -> return ((stA1, stB), Nothing)
      Just d -> do
        (stB1, me) <- roboB stB d
        return ((stA1, stB1), me)

robo321 :: Automata (Char, (Char, Char))
robo321 = gattai robo2 $ gattai robo2 robo1

-- 入力系列を喰わせて、出力を観察しよう

runAutomata :: Automata (Char, (Char, Char)) -> String -> (Maybe (Char, (Char, Char)), String)
runAutomata am str = (st, catMaybes res)
  where
    (st, res) = mapAccumL step (Just ('A',('A','A'))) str
    step Nothing _ = (Nothing, Nothing)
    step (Just st) c =
      case am st c of
        Nothing -> (Nothing, Nothing)
        Just (st, md) -> (Just st, md)

test1 = runAutomata robo321 "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" -- 029A
test2 = runAutomata robo321 "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" -- 980A
test3 = runAutomata robo321 "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 179A
test4 = runAutomata robo321 "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" -- 456A
test5 = runAutomata robo321 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 379A

{-
ghci> test1
(Just ('A',('A','A')),"029A")
ghci> test2
(Just ('A',('A','A')),"980A")
ghci> test3
(Just ('A',('A','A')),"179A")
ghci> test4
(Just ('A',('A','A')),"456A")
ghci> test5
(Just ('A',('A','A')),"379A")

うん、わりと形式的に作れて動いてるぞ。

もう少し確認
-}

runAutomata1 :: Automata (Char, (Char, Char)) -> (Char, (Char, Char)) -> String -> (Maybe (Char, (Char, Char)), String)
runAutomata1 am ini str = (st, catMaybes res)
  where
    (st, res) = mapAccumL step (Just ini) str
    step Nothing _ = (Nothing, Nothing)
    step (Just st) c =
      case am st c of
        Nothing -> (Nothing, Nothing)
        Just (st, md) -> (Just st, md)

test6 = [runAutomata1 robo321 ('A',('A',c)) "A" | c <- "0123456789A"]

{-
ということで、robo321は A-A-c で A を打ち込むと c を出力する。

('A',('A',"0-9A")) から開始して ('A',('A',"0-9A")) に遷移する最短の系列を全て求めたい
長さ0から順に全ての系列 5^len を作るというか幅優先探索して、
表が埋まるまでやるみたいな感じかな？
Nothingに落ちたやつを追跡しても無駄なので、ちゃんと枝刈りして進めたいが。

("^v<>A", ("^v<>A", "0-9A")) に "^v<>A" を与えるとどうなるか、という1ステップをrunAutomata1で計算できる

一つの文字cから開始して、全ての文字 0-9A で終わる最短経路を求める、を11回やる。

任意の状態をキー、任意の探索結果を値とするMapで探索済み状態を管理し、
外部から与える終了条件を満たすか、キューが空になったら終了し、
外部から与える状態遷移で探索を進める幅優先探索を作るか、専用のを書いて終わるか。

-}

searchFor c = loop done0 [(stAAc,"")] []
  where
    stAAc = ('A',('A',c))
    done0 = M.singleton stAAc ""
    doneZ = loop done0 [(stAAc,"")] []
    loop done [] [] = done
    loop done [] news = loop done news []
    loop done ((st,pl):ents) news = loop (M.union done $ M.fromList news1) ents (news1 ++ news)
      where
        news1 =
          [ (st1, pl ++ [d])
          | d <- "^v<>A"
          , Just (st1, Nothing) <- [robo321 st d]
          , M.notMember st1 done ]

searchAll = M.fromList
  [ ((c,d), pl)
  | c <- "0123456789A"
  , let m = searchFor c
  , d <- "0123456789A"
  , let pl = m M.! ('A',('A',d))]

{-
searchAllは ('A',('A',c)) から ('A',('A',d)) に遷移する最短操作が入っている

ひとつのコードを入力するには、
('A',('A','A')) から コード4文字を伝う最短操作の合計+それぞれの末尾にAが必要。
なので、concatMap ((++ "A") . (searchAll M.!)) $ zip ('A':code) code
-}

seq4code code = concatMap ((++ "A") . (searchAll M.!)) $ zip ('A' : code) code

complexity code = l * n
  where
    l = length $ seq4code code
    n = read $ init code

run1 i = readFile i >>= print . sum . map complexity . lines

samp1 = run1 "sample.txt"
main1 = run1 "input.txt"

{-
ghci> samp1
126384
ghci> main1
163920

つら。でも狙った通りにできてうれしい。

のも束の間、ロボの鎖が25に増えました、って何。
パート1の状態は 5*5*11
状況は、テンキー1、ロボが向かうカーソルキー2、自分が使うカーソルキー1、でいい？
なので、ロボの台数だけ状態が掛けるから、
11*5^25 = 3,278,255,462,646,484,375 状態。


トップからずっと、アームがAの上にある、の連鎖が続く範囲で、Aを押す操作が連鎖的に次の段へ入力を起こす。
それが途切れた層より下のところは、状態は変化しないので、全ての全てを 11*5^25 と計算する必要はない！

なので、力まかせに探索する、ただし全てを尽くす前に、ほしい状態が全て揃ったら抜ける、で何とかならないかな。
-}

-- 状態がタプルでなくリストで合体するオートマトン
type LAM = String -> Char -> Maybe (String, Maybe Char)

-- テンキーを触るロボ1
robo1s :: LAM
robo1s [st] c = do
  (st1,md) <- robo1 st c
  return ([st1], md)

cons :: Automata Char -> LAM -> LAM
cons roboA roboB (stA:stB) c = do
  (stA1, mc) <- roboA stA c
  case mc of
    Nothing -> return (stA1:stB, Nothing)
    Just d -> do
      (stB1, me) <- roboB stB d
      return (stA1:stB1, me)

robotrain = foldr cons robo1s $ replicate 25 robo2


runTrain2 str = (st, catMaybes res)
  where
    train2 = foldr cons robo1s [robo2, robo2]
    (st, res) = mapAccumL step (Just $ replicate 3 'A') str
    step Nothing _ = (Nothing, Nothing)
    step (Just st) c =
      case train2 st c of
        Nothing -> (Nothing, Nothing)
        Just (st, md) -> (Just st, md)

test11 = runTrain2 "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" -- 029A
test12 = runTrain2 "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" -- 980A
test13 = runTrain2 "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 179A
test14 = runTrain2 "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" -- 456A
test15 = runTrain2 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" -- 379A

{-
この先進めるのに、もうすこし軽くするために、
系列をそのまま持ち、しかも末尾に追加するのは無駄なので、長さだけをペイロードに入れること。

Aは、次もAなら伝播するが、そうでないときはそこで状態遷移を引き起こして完了する。
方向キーは、そこで状態遷移を引き起こして完了する。
そこより下については、任意の状態について共通となる。
というノードの同値関係を使って、把握するべき状態の個数を減らせないだろうか？

-}

searchForT c = loop done0 [(stAAc,"")] []
  where
    stAAc = replicate 25 'A' ++ [c]
    done0 = M.singleton stAAc ""
    doneZ = loop done0 [(stAAc,"")] []
    loop done [] news
      | null news = done -- まぁありえん
      | and [M.member (replicate 25 'A' ++ [d]) done | d <- "0123456789A"] = done -- 揃った！
      | otherwise = traceShow (M.size done) $ loop done news []
    loop done ((st,pl):ents) news = loop (M.union done $ M.fromList news1) ents (news1 ++ news)
      where
        news1 =
          [ (st1, pl ++ [d])
          | d <- "^v<>A"
          , Just (st1, Nothing) <- [robotrain st d]
          , M.notMember st1 done ]

-- naiveな方法では、状態が重ならなくてこの計算は止まらない。
