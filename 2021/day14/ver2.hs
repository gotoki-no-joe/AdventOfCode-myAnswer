{-
規則の CH->Bを、「列CHを列CBHに書き換える」と読むのではなく、
C-H という対を C-B と B-H という2対に置き換える、と読む。
これをすると、ほとんどの文字は *-X X-* の2つの対に属するために二重に数えるが、両端だけは数えないことに注意。
そして、1対が n 回のステップで どのような対の集団（総計 2^n 個）になるか、という表を作ることで、40ステップを32+8ステップで実行する。

文字はA-Zの全てを使うわけではないのだが、そこはちょっと手抜きで進めるか。


ファイルから読み込んだ規則は、StepMapに取り込むことができる。
これは1段の写像 singlestep である。
k段の写像を2度適用することで2k段の写像を作るには、
StepMapの各要素 stepmap ! (a,b) = inttable に対して、全ての要素についてstepmapを引いた結果を積和した表を作る。

-}

{-# LANGUAGE Strict #-}

import Data.Array

-- 対に対して割り当てをする表の型
type Table a = Array (Char,Char) a

-- ある分布状態
type State = Table Int

-- ステップマップは、個々の対に対して、置き換える2対を示す表を割り当てる表
type StepMap = Table State

--rng = ['A'..'Z']
--rng2 = [(a,b) | a <- rng, b <- rng]
aazz = (('A','A'),('Z','Z'))

-- Stateにstepmapを適用した次のStateを作る
apply :: StepMap -> State -> State
apply stepmap state = listArray aazz $ foldl (zipWith (+)) (repeat 0) [map (mag *) $ elems $ stepmap ! cd | (cd, mag) <- assocs state]

--double :: StepMap -> StepMap
--double stepmap = array aazz [(ab, listArray aazz $ foldl (zipWith (+)) (repeat 0) [map (mag *) $ elems $ stepmap ! cd | (cd,mag) <- assocs tbl]) | (ab,tbl) <- assocs sitemap]

-- step1とstep2を続けた結果のステップマップを作る
addMap :: StepMap -> StepMap -> StepMap
-- addMap step1 step2 = array aazz [(ab, listArray aazz $ foldl (zipWith (+)) (repeat 0) [map (mag *) $ elems $ step2 ! cd | (cd,mag) <- assocs tbl]) | (ab,tbl) <- assocs step1]
addMap step1 step2 = array aazz [(ab, apply step2 tbl) | (ab,tbl) <- assocs step1]

-- 同じものを指定すると倍にすることになる
doubleMap :: StepMap -> StepMap
doubleMap step = addMap step step

step10 step = addMap step8 step2
  where
    step2 = doubleMap step
    step8 = doubleMap $ doubleMap step2

step40 step = addMap step32 step8
  where
    step8 = doubleMap $ doubleMap $ doubleMap step
    step32 = doubleMap $ doubleMap step8

-- ファイルから読み込んだ結果は、
-- StepMapと、初期状態のStateと、両端の文字の組となる
parse :: String -> (StepMap, State, Char, Char)
parse xs = (singlestep, initial, head templ, last templ)
  where
    (templ:_:rs) = lines xs
    initial = accumArray (+) 0 aazz $ zip (zip templ $ tail templ) $ repeat 1
    singlestep = accumArray const nouse aazz $
                 [((a,b), accumArray (+) 0 aazz [((a, c), 1), ((c, b), 1)]) | (a:b:' ':'-':'>':' ':c:_) <- rs]

nouse = listArray aazz $ replicate (26*26) (-1000)

main1 xs = count last c1 c2
  where
    (singlestep, initial, c1, c2) = parse xs
    last = apply (step10 singlestep) initial

-- 文字を数える
count :: State -> Char -> Char -> Int
count state c1 c2 = maximum cs - minimum cs
  where
    al = [p | ((a,b),c) <- assocs state, p <- [(a,c),(b,c)]]
    aa = accumArray (+) 0 ('A','Z') $ (c1,1) : (c2,1) : al
    cs = map (flip div 2) $ elems aa

test1 = readFile "sample.txt" >>= print . main1

-- なんかメモリを食いつぶして帰ってこないのだけど、何をドジったかな。
-- Map (Char,Char) Int 方式にした方がよいだろうか。
-- ord c - ord 'A' で 0～25 にして IntMap がベストか？
