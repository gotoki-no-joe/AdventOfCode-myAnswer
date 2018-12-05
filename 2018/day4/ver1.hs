import Data.Char
import Data.List
import qualified Data.Map as M

{-
なかなかに面倒なデータ処理ですなぁ。
整列はそのまますればいいだろうけれど
（着任即寝るとかないだろうから。）
その先はどうする。
守衛IDごとにテーブルを用意して、
寝ている時間を++していく感じか。
どうせ0から59しか気にしないので、
そういうArrayでいくか。

それにしてもメンドクセェ。

一気に処理する代わりに、Guard の行を見てID覚えて、
salls asleepとwakes upの行にIDを付加する処理と、
...いやそこまで細切れにしなくてもいい。
ようは、falls asleepからwakes upの間だけを登録すればいいが、
それが誰なのかを先に知っておく必要がある、というだけだ。

type SleepData = IntMap (Array Int Int)

はめんどいので、重いの承知で

type SleepData = Map (Int,Int) Int

にしよう。
-}

main = do
  fi <- readFile "input.txt"
  let ls = sort $ lines fi
  let fstgrd = guardID (head ls)
  let thedata = buildSleepData M.empty fstgrd (tail ls)
  let persondata = M.mapKeysWith (+) fst thedata
  let theguard = mapMaxKey persondata
  print (theguard :: Int)
  let singledata = M.mapKeysWith (+) snd $ M.mapWithKey (\(id,m) cnt -> if id==theguard then cnt else 0) thedata
  let minute = mapMaxKey singledata
  print (minute :: Int)
  print $ theguard * minute
  putStrLn "part2"
  let ans2 = mapMaxKey thedata
  print ans2
  print $ uncurry (*) ans2

mapMaxKey :: (Ord a, Ord k) => M.Map k a -> k
mapMaxKey = snd . maximum . map (\(a,b) -> (b,a)) . M.toList

-- zeros = listArray (0,59) (replicate 60 0)

{-
[1518-03-28 00:04] Guard #2011 begins shift
.........|.........|......
-}

type SleepData = M.Map (Int,Int) Int

guardID :: String -> Int
guardID cs = read $ takeWhile isDigit $ drop 26 cs

minute :: String -> Int
minute cs = read $ take 2 $ drop 15 cs

buildSleepData :: SleepData -- 集約データ
               -> Int -- 先頭の守衛ID
               -> [String] -- 入力
               -> SleepData -- 結果
buildSleepData dat _ [] = dat
buildSleepData dat id (l:ls)
  | l !! 19 == 'G' = buildSleepData dat (guardID l) ls
  | otherwise =
    let
      m1 = minute l
      m2 = minute (head ls) - 1
      dat1 = M.unionWith (+) dat $ M.fromAscList [ ((id,m),1) | m <- [m1..m2] ]
    in
      buildSleepData dat1 id (tail ls)

{-
*Main> main
401
21
8421
part2
(2689,31)
83359
-}
