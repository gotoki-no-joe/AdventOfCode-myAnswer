import Data.List.Split
import Data.Array
import Data.List

runner i f = do
  ps <- map parse . lines <$> readFile i
  print $ f ps

parse :: String -> (String, [Int])
parse l = (l1, ks)
  where
    l1:l2:_ = words l
    ks = map read $ wordsBy (',' ==) l2

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 = sum . map score

score :: ([Char], [Int]) -> Int
score (l0, ks) = recur l ks
  where
    l = l0 ++ "." -- 番兵
    recur "" (_:_) = 0 -- 文字を使い切って数が残ったら失敗
    recur cs [] = if notElem '#' cs then 1 else 0 -- 数を使い切って#が残っていなければ成功
    recur ccs@(c:cs) kks@(k:ks) =
      case c of
        '.'             -> dotCase   -- '.' はただ読み飛ばす
        '#' | cond      -> sharpCase -- ここは成功、次の1文字も除いて次へ
            | otherwise -> 0         -- 連続kの#と続く空白を確保できない
        '?' | cond      -> dotCase + sharpCase
            | otherwise -> dotCase
      where
        dotCase = recur cs kks
        sharpCase = recur (tail cs2) ks
        (cs1, cs2) = splitAt k ccs
        cond = notElem '.' cs1 && -- k個までは`.`がなく
               not (null cs2) &&  -- 続きが残っていて
               head cs2 /= '#'    -- その先頭は`.`になれる

{-
ghci> test1
21
ghci> main1
7670
-}

score2 :: ([Char], [Int]) -> Int
score2 (l0, ks) = recur 0 0
  where
    llen = succ $ length l0
    carr = listArray (0, pred llen) $ l0 ++ "."
    klen = length ks
    karr = listArray (0, pred klen) ks

    recur i j
      | i == llen, j < klen = 0    -- 文字を使い切って数が残ったら失敗
      | j == klen, noSharp  = 1    -- 数を使い切って#が残っていなければ成功
      | j == klen      = 0         -- あれば失敗
      | c == '.'       = dotCase   -- '.' はただ読み飛ばす
      | c == '#', cond = sharpCase -- ここは成功、次の1文字も除いて次へ
      | c == '#'       = 0
      | c == '?', cond = dotCase + sharpCase
      | c == '?'       = dotCase
      where
        noSharp = and [carr ! ii /= '#' | ii <- [i .. pred llen]]
        c = carr ! i
        k = karr ! j
        dotCase   = recur (succ i) j
        sharpCase = recur (i + succ k) (succ j)
        ik = i + k
        cond = ik < llen &&
               and [carr ! ii /= '.' | ii <- [i .. pred ik]] &&
               carr ! ik /= '#'

part1a = sum . map score2

{-
ghci> runner "sample.txt" part1a
21
ghci> runner "input.txt" part1a
7670
-}

score3 :: ([Char], [Int]) -> Int
score3 (l0, ks) = arr ! (0, 0)
  where
    llen = succ $ length l0
    carr = listArray (0, pred llen) $ l0 ++ "."
    klen = length ks
    karr = listArray (0, pred klen) ks

    bnds = ((0,0), (llen, klen))
    arr = listArray bnds $ map recur $ range bnds
    recur (i, j)
      | i == llen, j < klen = 0    -- 文字を使い切って数が残ったら失敗
      | j == klen, noSharp  = 1    -- 数を使い切って#が残っていなければ成功
      | j == klen      = 0         -- あれば失敗
      | c == '.'       = dotCase   -- '.' はただ読み飛ばす
      | c == '#', cond = sharpCase -- ここは成功、次の1文字も除いて次へ
      | c == '#'       = 0
      | c == '?', cond = dotCase + sharpCase
      | c == '?'       = dotCase
      where
        noSharp = and [carr ! ii /= '#' | ii <- [i .. pred llen]]
        c = carr ! i
        k = karr ! j
        dotCase   = arr ! (succ i, j)
        sharpCase = arr ! (i + succ k, succ j)
        ik = i + k
        cond = ik < llen &&
               and [carr ! ii /= '.' | ii <- [i .. pred ik]] &&
               carr ! ik /= '#'

part1b = sum . map score3

extend (l, ks) = (l5, ks5)
  where
    l5 = intercalate "?" $ replicate 5 l
    ks5 = concat $ replicate 5 ks

part2 = sum . map (score3 . extend)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2
