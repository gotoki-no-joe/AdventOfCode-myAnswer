{-
どうやれば、うまく回り道ありの最良経路が見つけられるかな。

マスに対して、「到達に必要な既知の最小コスト」か maxBoundを割り当てる。
あるマスに到達できたとき、その周辺のマスに到達する続きのコストが決まる。
それが既知の値よりも安いなら、それでさらに続きを求める。

投入コストを1ずつ増やして、安いものから侵入すれば、一意に求められるが、ごちゃごちゃする。
到達するときのコストを優先度付きキューに入れて、順に処理すれば、重複の排除に気を付ければよくなる。
そんな感じかな。
-}

import Data.Array.IO
import Data.Array
import qualified Data.Heap as H
import Data.Char

compute2 :: Bool -> String -> IO Int
compute2 domag5 xs = do
  let ls = lines xs
  let dss = (if domag5 then mag5 else id) $ map (map digitToInt) ls
  let (w,h) = (length $ head dss, length dss)
  let cells = array ((1,1),(w,h)) [((i,j), d) | (j,ds) <- zip [1..] dss, (i,d) <- zip [1..] ds]
  costs <- newArray ((1,1),(w,h)) maxBound
  let pq = H.singleton (H.Entry 0 (1,1))
  loop cells costs pq
  readArray costs (w,h)

test2 = readFile "sample.txt" >>= compute2 True >>= print

run2 = readFile "input.txt" >>= compute2 True >>= print

incr 9 = 1
incr n = succ n

mag5 :: [[Int]] -> [[Int]]
mag5 dss = dss2
  where
    dss1 = map (concat . take 5 . iterate (map incr)) dss
    dss2 = concat $ take 5 $ iterate (map (map incr)) dss1

loop :: Array (Int,Int) Int -> IOArray (Int,Int) Int -> H.Heap (H.Entry Int (Int,Int)) -> IO ()
loop cells costs pq =
  case H.viewMin pq of
    Nothing -> return ()
    Just (H.Entry c xy, pq1) -> do
      c0 <- readArray costs xy
      pq2 <- if c0 <= c then return pq1 else do
        writeArray costs xy c
        return $ foldl (flip H.insert) pq1 [H.Entry (c + cells ! zw) zw | zw <- map (add xy) [(-1,0),(1,0),(0,-1),(0,1)], inbound zw (bounds cells)]
      loop cells costs pq2

add (x,y) (z,w) = (x+z, y+w)

inbound (x,y) ((x0,y0),(x1,y1)) = x0 <= x && x <= x1 && y0 <= y && y <= y1

test1 = readFile "sample.txt" >>= compute2 False >>= print

run1 = readFile "input.txt" >>= compute2 False >>= print

{-
*Main> test1
40
*Main> run1
769
*Main> test2
315
*Main> run2
2963
-}
