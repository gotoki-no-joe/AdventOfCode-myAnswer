import Data.Char
import Data.List
import qualified Data.Map as M

{-
同時刻に複数のイベントがないと勝手に仮定して（sortコマンドで検証できる）
字面でのソートで時系列順にする。

ファイルの形式
[1518-03-28 00:04] Guard #2011 begins shift
.........|.........|......
時刻は16文字めから2文字、
IDは27文字めから空白まで、
20文字めがGなら着任、さもなくば寝たと起きたの対（区別しなくてももわかる）

守衛IDと時刻の対に対して、寝ていた回数をカウントする
type SleepData = Map (Int,Int) Int
-}

main = do
  fi <- readFile "input.txt"
  let ls = sort $ lines fi
  let thedata = buildSleepData ls
  let ans1 = part1 thedata
  print ans1
  print $ uncurry (*) ans1
  putStrLn "part2"
  let ans2 = maxKey thedata
  print ans2
  print $ uncurry (*) ans2

type SleepData = M.Map (Int,Int) Int

guardID :: String -> Int
guardID cs = read $ takeWhile isDigit $ drop 26 cs

minute :: String -> Int
minute cs = read $ take 2 $ drop 15 cs

buildSleepData :: [String] -> SleepData
buildSleepData ls = loop ls M.empty undefined
  where
    -- 入力 累積データ 現在の守衛
    loop :: [String] -> SleepData -> Int -> SleepData
    loop [] dat _ = dat
    loop (l:ls) dat id
      | l !! 19 == 'G' = loop ls dat (guardID l)
      | otherwise =
        let
          m1 = minute l
          m2 = minute (head ls) - 1
          dat1 = M.unionWith (+) dat $ M.fromAscList [((id,m),1) | m <- [m1..m2]]
        in
          loop (tail ls) dat1 id

{-
part1
最も眠っている守衛を見つけるには、時刻についてSleepDataを畳み込み、最大値のIDをとる
次に、その守衛のデータだけ抜き出し、最大値の時刻をとる
-}

part1 :: SleepData -> (Int,Int)
part1 thedata = maxKey singledata
  where
    persondata = M.mapKeysWith (+) fst thedata
    theguard = maxKey persondata
    singledata = M.filterWithKey (\(id,_) _ -> theguard == id) thedata

maxKey :: (Ord a, Ord k) => M.Map k a -> k
maxKey = snd . maximum . map (\(a,b) -> (b,a)) . M.toList

{-
*Main> main
(401,21)
8421
part2
(2689,31)
83359
-}
