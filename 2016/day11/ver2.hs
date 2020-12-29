import Data.Word
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Debug.Trace

{-
2020/12/27

状態を、
エレベータのあるフロア ev :: Word8 0～3
フロアにあるchipの情報 chips :: Vector Word8
フロアにあるgeneratorの情報 gens :: Vector Word8

bit0から順に、そこにあるとき1とする。冗長ではある。

この形式は、違反チェックがやりやすいけど、状態の生成がやりにくい。

別の状態表現
エレベータのあるフロア ev :: Word8 0～3
それぞれのchipとgeneratorのあるフロア items :: Vector Word8
　偶数番と奇数番で対にする。

生成は、evと等しい番号のindexを1つまたは2つ選んでevと同時にincdecする。
検査は、それぞれのフロアについて、genのないchipがあって、chipのないgenがあったらアウト。
なんか冗長くさい。フロアのビットベクトルを考えて、genとchipが異なるフロアにあるときそれぞれを1にする
両方立ってるフロアがあったらアウト、なら1passになる。これだ。
-}

{- sample
The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM
-}

data State = State { ev :: Word8, items :: V.Vector Word8 } deriving (Eq, Ord, Show)

sample, sampleGoal :: State
sample = State { ev = 0, items = V.fromList [1,0,2,0] }
sampleGoal = State { ev = 3, items = V.fromList [3,3,3,3] }

{-
isValid :: State -> Bool
isValid st = V.notElem (True,True) defsV
  where
    f (a,b) (c,d) = (a || c, b || d)
    defsV = V.accum f (V.replicate 4 (False,False)) defects
    defects = concat [ [(a,(True,False)),(b,(False,True))]
                     | i <- [0,2..pred $ V.length $ items st]
                     , let a = fromIntegral $ items st V.! i, let b = fromIntegral $ items st V.! succ i
                     , a /= b]

修正
GenのないChipはあぶない
Genはあれば常に危険
-}

isValid :: State -> Bool
isValid st = V.notElem (True,True) defsV
  where
    f (a,b) (c,d) = (a || c, b || d)
    defsV = V.accum f (V.replicate 4 (False,False)) defects
    defects = concat [ [(b,(True,False)) | a /= b] ++ [(a,(False,True))]
                     | i <- [0,2..pred $ V.length $ items st]
                     , let a = fromIntegral $ items st V.! i, let b = fromIntegral $ items st V.! succ i]

stepMoves :: State -> [State]
stepMoves st =
    [ st2
    | fl2 <- [pred fl1 | fl1 > 0] ++ [succ fl1 | fl1 < 3]
    , cs <- choices
    , let st2 = State { ev = fl2, items = itms V.// [(j,fl2) | j <- cs]}
    , isValid st2
    ]
  where
    fl1 = ev st
    itms = items st
    idxs = [ j | j <- [0..pred $ V.length itms], itms V.! j == fl1 ]
    choices = [ [j] | j <- idxs] ++ [[j,k]|j:js <- tails idxs, k <- js]

{- 普通に幅優先探索する
既知の全ての状態、探索するべきフレッシュな状態の二つを持ちまわる。

フレッシュな状態から、stepMovesで新しい枝を伸ばしたもので、まだ見たことのないものを全て集める
-}

bfs :: State -> Int -> S.Set State -> S.Set State -> Int
bfs goal cnt visited sts
  | S.null sts = error "Oops."
  | S.member goal sts = {- trace (show sts) $ -} cnt
  | True = {- trace (show sts) $ -} trace (unwords [show cnt, show $ S.size sts]) $ bfs goal (succ cnt) visited1 (S.foldr f S.empty sts)
  where
    visited1 = S.union visited sts
    f st sts1 = S.union sts1 $ S.fromList [st1 | st1 <- stepMoves st, S.notMember st1 visited1]

test1 = bfs sampleGoal 0 S.empty (S.singleton sample)

{- そして11なはずなのに9という答えになってしまった。why? -}

bfsM :: State -> M.Map State [State] -> M.Map State [State] -> [State]
bfsM goal visited sts
  | M.null sts = error "Oops."
  | M.member goal sts = reverse (sts M.! goal)
  | True = bfsM goal visited1 (M.foldrWithKey f M.empty sts)
  where
    visited1 = M.union visited sts
    f st history sts1 = M.union sts1 $ M.fromList [(st1,st1:history) | st1 <- stepMoves st, M.notMember st1 visited1]

test1m = bfsM sampleGoal M.empty (M.singleton sample [sample])

{-

条件が間違ってた。これうざいなぁ。-

Generatorは危険。同じ階のChipが死ぬ。
Chipは、同じGeneratorと同じ階にあるときは安全。他のGeneratorがあっても。
Chipがあっても、Generatorは安全にはならない。
...そういうことか。

isValidのdefectsをGenがあればChipがあっても危険、に修正

さらに左右が違っていたのを直した。

-}

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

mydata :: State
mydata = State { ev = 0, items = V.fromList [0,0,1,2,1,2,1,2,1,2] }
mygoal = State { ev = 3, items = V.fromList [3,3,3,3,3,3,3,3,3,3] }

ans1 = bfs mygoal 0 S.empty (S.singleton mydata)

{-
*Main> ans1
0 1
1 2
2 16
3 34
4 182
5 260
6 929
7 1128
8 2448
9 2729
10 4215
11 4874
12 7565
13 7660
14 10441
15 10326
16 12911
17 13627
18 11440
19 14725
20 8786
21 9816
22 8196
23 6005
24 5131
25 4035
26 3900
27 2266
28 1991
29 1140
30 756
31 405
32 165
33
かなりかかった。そしたパート2は...
-}

mydata2 :: State
mydata2 = State { ev = 0, items = V.fromList [0,0,1,2,1,2,1,2,1,2,0,0,0,0] }
mygoal2 = State { ev = 3, items = V.fromList [3,3,3,3,3,3,3,3,3,3,3,3,3,3] }

ans2 = bfs mygoal2 0 S.empty (S.singleton mydata2)

{-
*Main> ans2
0 1
1 3
2 33
3 129
4 545
5 3075
6 5595
7 20872
8 26005
9 76347
10 70491
11 121511
12 129344
13 147738
14 179592
15 165273
16 226795
17 195299
18 277331
19 255857
20 287391
21 289970
22 270310
23 280730
24 244164
25 241696
26 202150
27 210609
28 171458
29 206923
30 151446
31 184535
32 155470
33 177233
34 156421
35 147736
36 145329
37 111088
38 115118
39 76877
40 82803
41 49163
42 55192
43 29262
44 33614
45 15159
46 18004
47 7796
48 9389
49 4065
50 4472
51 1365
52 1751
53 728
54 805
55 189
56 203
57

なんとかパンクせずに完了した。
-}

{-
evと要素14個で 2bit * 15 = 30bitなので、ビット押し込みで32bit計算機でもいける範囲だったんだ。
-}
