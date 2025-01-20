-- 2025/1/16 for spoiler

{-
ユニバーサルな幅優先探索
初期状態リスト
次状態を生成する関数（ここで禁止状態も捨てる）
目標の状態（が含まれるかを判定する関数、まで一般化する必要はない？、のではなく、効率的にできない
けどどうせ、もう一周するなら全部の要素を舐めるのだし、いいのか？）
Setに入れるためのOrd
EqやOrdは、純粋な状態に、そこに至る手順を付記したとき、
それを無視して比較するような計算をあてることが考えられる。それはよい？

それを利用者の型に押しつけるよりは、それをTupleにしつつ動作するbfsの亜種があればそれでいいのでは。
-}
import qualified Data.Set as S

import Data.Word
import qualified Data.Vector.Unboxed as UV
import Data.List
import Data.Bits

import Data.Time.Clock

bfs :: Ord a => [a] -> (a -> [a]) -> a -> (Either Int Int, Int)
bfs inits gen goal = loop 0 st0 st0
  where
    st0 = S.fromList inits
    loop cnt visited sts
      | S.null sts        = (Left cnt , S.size visited)  -- 行き詰まった
      | S.member goal sts = (Right cnt, S.size visited) -- 到達した
      | otherwise         = loop (succ cnt) visited1 sts1
      where
        sts1 = S.fromList [st1 | st <- S.elems sts, st1 <- gen st, S.notMember st1 visited]
        visited1 = S.union visited sts1

-- Word8を2k+1個のベクトル
-- 0がエレベータ、2i-1が発電機、2iがチップ

type ModeA = UV.Vector Word8

sample0a, sampleGoala :: ModeA
sample0a    = UV.fromListN 5 [0, 1, 0, 2, 0]
sampleGoala = UV.fromListN 5 [3, 3, 3, 3, 3]
genModeA :: ModeA -> [ModeA]
genModeA st =
  [ st UV.// [(0, ev1), (a, ev1), (b, ev1)]
  | (a,b) <- sels
  , isSafe $ delete a $ delete b $ is -- aとbを持ち出してもev階は安全
  , ev1 <- [pred ev | ev > 0] ++ [succ ev | ev < 3] -- 移動先
  , isSafe $ a : b : [i | (i,f) <- zip [1..] fs, f == ev1] -- エレベータの移動先はa,bを持ち込んでも安全
  ]
  where
    ev:fs = UV.toList st                -- それぞれの位置
    is = [i | (i,f) <- zip [1..] fs, f == ev]     -- evと同じ階のアイテムリスト
    sels = [(a,b) | bs@(a:_) <- tails is, b <- bs] -- 次に持ち出すもの候補
    isSafe is = not (any odd is) || -- 発電機が一つもないならヨシ
                all (flip elem is . pred) (filter even is) -- チップは発電機があるならヨシ

test1a = bfs [sample0a] genModeA sampleGoala

-- 一瞬でした。

{- input.txt
The first floor contains a promethium generator and a promethium-compatible microchip.
The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
The fourth floor contains nothing relevant.

4 :
3 :            CoM     CuM     RM    PlM
2 :                CoG     CuG    RG     PlG
1 : Ev PrM PrG
-}

input0a, inputGoala :: ModeA
input0a    = UV.fromListN 11 [0, 0, 0, 1, 2, 1, 2, 1, 2, 1, 2]
inputGoala = UV.fromListN 11 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]

main1a = bfs [input0a] genModeA inputGoala

{-
ghci> main1a
Right 33
(23.62 secs, 17,685,343,584 bytes)
-}

input0ap2, inputGoalap2 :: ModeA
input0ap2    = UV.fromListN 15 [0, 0, 0, 1, 2, 1, 2, 1, 2, 1, 2,0,0,0,0]
inputGoalap2 = UV.fromListN 15 [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,3,3,3,3]

main2a = bfs [input0ap2] genModeA inputGoalap2

{-
> ghc -O2 ver3
> ./ver3
(Right 57,6042507)

6百万状態を探索した。コンパイルしないとつらい。
-}

type ModeB = UV.Vector Word16

-- 下位7ビットがチップ、8ビットめから7ビットが発電機、ビット15がエレベータ
sample0b, sampleGoalb :: ModeB
sample0b    = UV.fromListN 4 [3 + bit 15, 1 .<<. 8, 2 .<<. 8, 0]
sampleGoalb = UV.fromListN 4 [0,0,0,3 + 3 .<<. 8 + bit 15]
genModeB :: ModeB -> [ModeB]
genModeB bits =
  [ bits UV.// [(ev, bitsev1),(ev1,bitsev11)]
  | ab <- sels
  , let bitsev1 = bitsev .^. ab
  , isSafe bitsev1
  , ev1 <- [pred ev | ev > 0] ++ [succ ev | ev < 3] -- 移動先
  , let bitsev11 = (bits UV.! ev1) .^. ab
  , isSafe bitsev11
  ]
  where
    ev = fst $ head $ filter (flip testBit 15 . snd) $ zip [0 ..] $ UV.toList bits
    bitsev = bits UV.! ev
    is = [i | i <- [0 .. 14], testBit bitsev i]
    sels = [bit 15 .|. bit a .|. bit b | bs@(a:_) <- tails is, b <- bs]
    isSafe bit = gens == 0 || chips .&. complement gens == 0
      where
        chips = bit .&. 127
        gens  = (bit .>>. 8) .&. 127

test1b = bfs [sample0b] genModeB sampleGoalb

input0b, inputGoalb :: ModeB
input0b    = UV.fromListN 4 [1 + 1 .<<. 8 + bit 15, 30 .<<. 8, 30, 0]
inputGoalb = UV.fromListN 4 [0,0,0,31 + 31 .<<. 8 + bit 15]

main1b = bfs [input0b] genModeB inputGoalb

{-
ghci> test1a
(Right 11,353)
(0.02 secs, 8,756,528 bytes)
ghci> test1b
(Right 11,353)
(0.01 secs, 6,283,008 bytes)

ghci> main1a
(Right 33,158186)
(32.76 secs, 17,685,986,832 bytes)
ghci> main1b
(Right 33,158186)
(17.04 secs, 7,580,713,336 bytes)

はやっ！圧倒的？
-}

input0bp2, inputGoalbp2 :: ModeB
input0bp2    = UV.fromListN 4 [1 + 1 .<<. 8 + bit 15 + 3 .<<. 5 + 3 .<<. (5 + 8), 30 .<<. 8, 30, 0]
inputGoalbp2 = UV.fromListN 4 [0,0,0,127 + 127 .<<. 8 + bit 15]

main2b = bfs [input0bp2] genModeB inputGoalbp2

main = do
  t0 <- getCurrentTime
  print main2a
  t1 <- getCurrentTime
  print $ diffUTCTime t1 t0
  print main2b
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

{-
> ghc -O2 ver3
> ./ver3
(Right 57,6042507)
68.7723915s
(Right 57,6042507)
29.3781851s
-}
