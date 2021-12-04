import Data.List.Split
import Data.Array
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Debug.Trace

main1 fn = do
  co <- readFile fn
  let (l1:ls) = lines co
  let balls = map read $ wordsBy (',' ==) l1
  let cards = map (map (map read . words) . tail) $ chunksOf 6 ls
  let ans = compute1 balls cards
  print ans

test1 = main1 "sample.txt"
run1 = main1 "input.txt"

data BingoLine
  = R1 | R2 | R3 | R4 | R5
  | C1 | C2 | C3 | C4 | C5
--  | DI | DJ
  deriving (Eq,Enum,Ord,Ix,Show)

type CardState = (IM.IntMap [BingoLine], Array BingoLine Int)

newCard :: [[Int]] -> CardState
newCard xss = (m, a)
  where
    a = accumArray (+) 0 ( R1,  C5 {-DJ-}) []
    m = IM.fromList [(x, f r c) | (r,xs) <- zip [0..] xss, (c,x) <- zip [0..] xs]
    f r c = {- DI | r == c] ++ [DJ | r + c == 4] ++ -} [[R1 .. R5] !! r, [C1 .. C5] !! c]

numberCall :: Int -> CardState -> CardState
numberCall n ma@(m, a) =
  case IM.lookup n m of
    Nothing -> ma
    Just bls -> (m, accum (+) a [(l,1) | l <- bls])

winCheck :: CardState -> Bool
winCheck (_, a) = elem 5 $ elems a

compute1 :: [Int] -> [[[Int]]] -> Int
compute1 balls cardsdat = trace (show theCard) $ score (take (length cs1) balls) theCard
  where
    cards = map newCard cardsdat
    (cs1,cs:_) = span (not . any winCheck) $ scanl step cards balls
    theCard = head $ filter winCheck cs

step cards ball = map (numberCall ball) cards

score :: [Int] -> CardState -> Int
score balls (m,_) = trace (show balls) $ trace (show x) $ last balls * x
  where
    x = IS.foldl' (+) 0 $ IS.difference (IM.keysSet m) (IS.fromList balls)

{-
それぞれのカードについて、何手めにビンゴして、得点はいくつかを計算させる、だと、ビンゴしない場合に困る。
途中までのシーケンスで「これでビンゴしているか？」を聞くのだと、毎回先頭からするのもナニな感じ。うーん？
「途中まで進めたビンゴカード」をきれいに表す方法が欲しい。

縦1列め～5列め、横1行め～5行め、斜め、逆の斜め、というenumerateを用意して、
数に対して、それがどこに影響するかの、長さ1～3のリストを割り当てるMapでカードを表す。
そして、いくつ穴が開いたかをカウントするenumerateからintへのarrayを用意して、
これが5になったらアガリ。

スコアは、カードの数の集合（Mapのキーの集合）から出現した番号の集合を引いてやれば求められる。

なんだか変だと思ったら、斜め無しかよ！
-}

main2 fn = do
  co <- readFile fn
  let (l1:ls) = lines co
  let balls = map read $ wordsBy (',' ==) l1
  let cards = map (map (map read . words) . tail) $ chunksOf 6 ls
  let ans = compute2 balls cards
  print ans

test2 = main2 "sample.txt"
run2 = main2 "input.txt"

compute2 :: [Int] -> [[[Int]]] -> Int
compute2 balls cardsdat = trace (show theCard) $ score (take k balls) (numberCall (balls !! k) theCard)
  where
    cards = map newCard cardsdat
    cs1 = takeWhile (not . all winCheck) $ scanl step cards balls
    theCard = head $ filter (not . winCheck) (last cs1)
    k = length cs1
