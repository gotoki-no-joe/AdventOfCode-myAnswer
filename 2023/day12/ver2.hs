import Control.Parallel.Strategies

import Data.List.Split
import Data.List

import Data.Array

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
score (l, ks) = recur (length l) l ks
  where
    recur 0 "" (_:_) = 0 -- 文字を使い切って数が残ったら失敗
    recur _ cs [] = if notElem '#' cs then 1 else 0 -- 数を使い切って#が残っていなければ成功
    recur l ccs@(c:cs) kks@(k:ks) =
      case c of
        '.' -> dotCase -- '.' はただ読み飛ばす
        '#' | l == k, cond1 -> recur 0 "" ks  -- kをちょうど入れきれるならここは成功
            | l > k, cond1, cond2 -> recur (l - succ k) (tail cs2) ks -- ここは成功、次の1文字も除いて次へ
            | otherwise -> 0 -- 文字が足らない、.が混じる、k+1文字めが.でない、など
        '?' | l == k, cond1 -> recur 0 "" ks + dotCase
            | l > k, cond1, cond2 -> recur (l - succ k) (tail cs2) ks + dotCase
            | otherwise -> dotCase
      where
        dotCase = recur (pred l) cs kks
        (cs1, cs2) = splitAt k ccs
        cond1 = notElem '.' cs1
        cond2 = head cs2 /= '#'

{-
# に遭遇したとき、数を使うしかない。
続くk個が . でない文字かつ、その次が#でない文字なら、そこまで消費して成功
条件を満たさない場合は失敗
数がないときも失敗

. は、飛ばして次に行くしかない

? に遭遇したとき、
数がないとき、. と見なして飛ばすしか選択肢はない。
数があっても、飛ばすという選択肢は一つある。
飛ばさないで、ここから数を始めるとき、同様に判定する。

数を使い切りつつ、文字列を使い切れたとき1成功カウント、を数える。
-}

{-
ghci> test1
21
ghci> main1
7670
-}

-- part 2

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 lks = sum $ runEval $ parList rpar $ map score2 lks

score2 (l, ks) = score (l5, ks5)
  where
    l5 = intercalate "?" $ replicate 5 l
    ks5 = concat $ replicate 5 ks

{-
1000行ある問題を Parallel.Strategies で何とかしようという。
-}

main = main2

-- コンパイルしても無理だったので、DPにする。
-- 文字列をx文字目まで、数値列をy個まで消費した状態に到達する方法の場合の数を数える。
-- 状態を無駄に増やさないため、連続する . を一つに置き換える前処理を入れよう。

-- というか、scoreの引数を、文字列と数字列の現在位置にすれば、配列DPにそのまま乗せられる。

arrayDP :: Ix i => (i, i) -> (i -> ([i], [(i, b)] -> b)) -> Array i b
arrayDP bnds nf = dpArr
  where
    dpArr = listArray bnds [fi [(j, dpArr ! j) | j <- js] | i <- range bnds, let (js, fi) = nf i]

score3 :: ([Char], [Int]) -> Int
score3 (l, ks) = dparr ! (0, 0)
  where
    ll = l ++ "." -- 番兵
    llen = length ll
    klen = length ks
    dparr = arrayDP ((0, 0), (llen, klen)) f
    f :: (Int,Int) -> ([(Int,Int)], [((Int,Int),Int)]->Int)
    f (i, j)
      | i == llen, j < klen = ([], const 0)  -- 文字を使い切って数が残ったら失敗
      | j == klen, notElem '#' $ drop i ll = ([], const 1) -- 数を使い切って残りに # がなければ成功
      | j == klen = ([], const 0) -- あるなら失敗
      | c == '.' = ([(succ i, j)], snd . head) -- '.' は単に読み飛ばす
      | c == '#', i + k < llen, noDot, yesDot = ([(i+succ k, succ j)], snd . head) -- kが入って続きがこれる
      | c == '#' = ([], const 0) -- 入らないなら失敗
      | c == '?', i + k < llen, noDot, yesDot = ([(i + succ k, succ j), (succ i, j)], sum . map snd)
      | c == '?' = ([(succ i, j)], snd . head)
      where
        c = ll !! i
        k = ks !! j
        noDot = notElem '.' $ take k $ drop i ll -- 現在位置からk文字に '.' がない
        yesDot = '#' /= ll !! (i + k) -- その次に '.' になれる文字がある

part1a = sum . map score3
part2a = sum . map (score3 . extend)

extend (l, ks) = (l5, ks5)
  where
    l5 = intercalate "?" $ replicate 5 l
    ks5 = concat $ replicate 5 ks

-- 無理に配列DPの汎用版を使うより、手で書いた方が見通しいいなこれ。

score4 (l0, ks) = arr ! (0, 0)
  where
    l = l0 ++ "." -- 番兵
    llen = length l
    klen = length ks
    bnds = ((0,0), (llen, klen))
    arr = listArray bnds $ map f $ range bnds
    f (i, j)
      | i == llen, j < klen = 0  -- 文字を使い切って数が残ったら失敗
      | j == klen, notElem '#' $ drop i l = 1 -- 数を使い切って残りに # がなければ成功
      | j == klen = 0 -- あるなら失敗
      | c == '.' = arr ! (succ i, j) -- '.' は単に読み飛ばす
      | c == '#', i + k < llen, noDot, yesDot = arr ! (i+succ k, succ j) -- kが入って続きがこれる
      | c == '#' = 0 -- 入らないなら失敗
      | c == '?', i + k < llen, noDot, yesDot = arr ! (i+succ k, succ j) + arr ! (succ i, j)
      | c == '?' = arr ! (succ i, j)
      where
        c = l !! i
        k = ks !! j
        noDot = notElem '.' $ take k $ drop i l -- 現在位置からk文字に '.' がない
        yesDot = '#' /= l !! (i + k) -- その次に '.' になれる文字がある

part1b = sum . map score4
part2b = sum . map (score4 . extend)

{-
ghci> runner "sample.txt" part2b
525152
ghci> runner "input.txt" part2b
157383940585037
-}
