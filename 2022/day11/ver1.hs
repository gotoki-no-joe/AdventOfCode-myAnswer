{-
サルは7匹なので、ハードコーディングしてしまうのが早いかな。

サルは、手持ちのアイテムキューと、これまでに投げたアイテムの個数という状態を持つ。
キューの初期値も与えられる。
アイテムのパラメータ操作と、投げる先の計算も与えられる。
これらの対でサルを表し、一連のシミュレーションを20ラウンド実行するシミュレータを書くことになるのか。
-}

import Control.Monad
import Data.Array.IO
import qualified Data.Sequence as Q
import Data.Foldable
import Data.List

-- 例題

-- type WorryLevel = Integer
type WorryLevel = Int
data Monkey = Monkey { si :: [WorryLevel], op :: WorryLevel -> WorryLevel, tst :: WorryLevel, t :: Int, f :: Int }
testData =
  [ Monkey [79, 98] (19 *) 23 2 3
  , Monkey [54, 65, 75, 74] (6 +) 19 2 0
  , Monkey [79, 60, 97] (^ 2) 13 1 3
  , Monkey [74] (3 +) 17 0 1
  ]

-- うんしかし、状態遷移があるので、命令型言語の方がやりやすそうだなぁ。
-- 模倣するためにSTモナドでSTArrayでやるか。

-- サルの0からmaxまで順に、
-- キューの長さのぶんだけ、カウントを増やす
-- キューの内容を、規則に従って配分する
-- 自分のキューを空にする
-- が1ラウンド。

-- IOArrayでやろう。

phase1 :: [Monkey] -> IO ()
phase1 ms =
  do
    qv <- newListArray (0, nM) $ map (Q.fromList . si) ms :: IO (IOArray Int (Q.Seq WorryLevel))
    cv <- newArray (0, nM) 0 :: IO (IOArray Int Int)
    forM_ [1..20] (\round -> do
      forM_ (zip [0..] ms) (\(i, m) -> do
        q <- readArray qv i
        readArray cv i >>= writeArray cv i . (Q.length q +)
        writeArray qv i Q.empty
        forM_ (toList q) (\item -> do
            let item1 = div (op m item) 3
            let j = if mod item1 (tst m) == 0 then t m else f m
            readArray qv j >>= writeArray qv j . (Q.|> item1)
--            print ("monkey ", i, item, item1, j)
            )
        )
      print ("round ", round)
      md <- getElems qv
      mapM_ print md
      )
    cnts <- getElems cv
    print $ product $ take 2 $ sortBy (flip compare) cnts
  where
    nM = pred $ length ms

inputData =
  [ Monkey [63, 84, 80, 83, 84, 53, 88, 72] (11 *) 13 4 7
  , Monkey [67, 56, 92, 88, 84] (4 +) 11 5 3
  , Monkey [52] (^ 2) 2 3 1
  , Monkey [59, 53, 60, 92, 69, 72] (2 +) 5 5 6
  , Monkey [61, 52, 55, 61] (3 +) 7 7 2
  , Monkey [79, 53] (1 +) 3 0 6
  , Monkey [59, 86, 67, 95, 92, 77, 91] (5 +) 19 4 0
  , Monkey [58, 83, 89] (19 *) 17 2 1
  ]

part1 = phase1 inputData

phase2 :: [Monkey] -> Int -> IO ()
phase2 ms times =
  do
    qv <- newListArray (0, nM) $ map (Q.fromList . si) ms :: IO (IOArray Int (Q.Seq WorryLevel))
    cv <- newArray (0, nM) 0 :: IO (IOArray Int Int)
    forM_ [1..times] (\round -> do
      forM_ (zip [0..] ms) (\(i, m) -> do
        q <- readArray qv i
        readArray cv i >>= writeArray cv i . (Q.length q +)
        writeArray qv i Q.empty
        forM_ (toList q) (\item -> do
            let item1 = mod (op m item) base
            let j = if mod item1 (tst m) == 0 then t m else f m
            readArray qv j >>= writeArray qv j . (Q.|> item1)
--            print ("monkey ", i, item, item1, j)
            )
        )
--      print ("round ", round)
--      md <- getElems qv
--      mapM_ print md
      )
    cnts <- getElems cv
    print cnts
    print $ product $ take 2 $ sortBy (flip compare) cnts
  where
    nM = pred $ length ms
    base = foldl1 lcm $ map tst ms

{-
単純に Integer にすると、1000回で吹っ飛ぶ。
サル全員のtstのlcmまたはproductをとって、この値でmodしても変わらない、かな？
-}

part2 = phase2 inputData 10000
