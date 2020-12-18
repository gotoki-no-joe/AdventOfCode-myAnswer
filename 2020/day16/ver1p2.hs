{-
パート2のために全てを作り直す
-}

import Data.Char
import Data.List

sample = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"

-- 規則名と、範囲ふたつのリスト
parseRule :: String -> (String,[(Int,Int)])
parseRule line = (xs0, [(read xs2,read xs4),(read xs6,read $ tail xs7)])
  where
    (xs0,xs1) = span (':' /=) line
    (xs2,xs3) = span isDigit $ drop 2 xs1
    (xs4,xs5) = span isDigit (tail xs3)
    (xs6,xs7) = span isDigit $ dropWhile (not.isDigit) xs5

parseTicket :: String -> [Int]
parseTicket line = read ('[': line ++ "]")

-- 先頭が自分の切符
getData co = (map parseRule rules, map parseTicket $ myt : drop 2 ls2)
  where
    (rules,ls1) = span (not.null) $ lines co
    (myt:ls2) = drop 2 ls1

comp2 (rss, tss) =
    [ (rn, map fst $ filter (isOKwith rs. snd) tcs)
    | (rn,rs) <- rss
    ]
  where
-- どの範囲にも合わない項目を持つ行を捨てる
    rs = concatMap snd rss
    isValid t = any (\(a,b) -> a <= t && t <= b) rs
    tss1 = filter (all isValid) tss
-- 欄ごとにデータを分け直す
    tcs = zip [0..] $ transpose tss1
-- 規則ごとに、何番目のカラムが全員合格かのリストを作る
    isOKwith rs tc = all (\t -> any (\(a,b) -> a <= t && t <= b) rs) tc

test1 = comp2 $ getData sample

exec1 = readFile "input.txt" >>= print . comp2 . getData

{-
*Main> exec1
[("departure location",[0,2,3,5,6,7,10,12,15,16,17])
,("departure station",[0,2,3,5,6,7,9,10,11,12,14,15,16,17])
,("departure platform",[0,2,3,5,6,7,9,10,11,12,13,14,15,16,17])
,("departure track",[0,2,3,5,6,7,9,10,12,15,16,17])
,("departure date",[0,2,3,5,6,7,9,10,11,12,15,16,17])
,("departure time",[0,1,2,3,5,6,7,9,10,11,12,13,14,15,16,17])
,("arrival location",[0,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18])
,("arrival station",[0,5,7,10])
,("arrival platform",[0,3,5,6,7,10,12,15,16,17])
,("arrival track",[0,5,7])
,("class",[0,5,6,7,10,15])
,("duration",[0,5])
,("price",[5])
,("route",[0,3,5,6,7,10,15,16,17])
,("row",[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19])
,("seat",[0,5,6,7,10,15,17])
,("train",[0,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17])
,("type",[0,3,5,6,7,10,15,17])
,("wagon",[0,5,6,7,10])
,("zone",[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18])]

もう手作業にしていい？まず price = 5 が決まる。他から5を除く。
,("price",[5])
,("duration",[0])

[("departure location",[2,3,6,7,10,12,15,16,17])
,("departure station",[2,3,6,7,9,10,11,12,14,15,16,17])
,("departure platform",[2,3,6,7,9,10,11,12,13,14,15,16,17])
,("departure track",[2,3,6,7,9,10,12,15,16,17])
,("departure date",[2,3,6,7,9,10,11,12,15,16,17])
,("departure time",[1,2,3,6,7,9,10,11,12,13,14,15,16,17])
,("arrival location",[1,2,3,4,6,7,9,10,11,12,13,14,15,16,17,18])
,("arrival station",[7,10])
,("arrival platform",[3,6,7,10,12,15,16,17])
,("arrival track",[7])
,("class",[6,7,10,15])
,("route",[3,6,7,10,15,16,17])
,("row",[1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19])
,("seat",[6,7,10,15,17])
,("train",[1,2,3,4,6,7,9,10,11,12,13,14,15,16,17])
,("type",[3,6,7,10,15,17])
,("wagon",[6,7,10])
,("zone",[1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18])]

途中で止まることなく行けそうだから、この先もコード化しないとだめっぽい。
singletonを探して、それを残りの要素のリストから全て除く、を、無くなるまで繰り返す。
-}

singleton [_] = True
singleton _ = False

refine ncs = loop [] ncs
  where
    loop done ncs
      | null ncs1 = (done,ncs)
      | True = loop (ncs1 ++ done) (map f ncs2)
      where
        (ncs1,ncs2) = partition (singleton . snd) ncs
        news = concatMap snd ncs1
        f (n,l) = (n, l \\ news)

test2 = refine test1

exec2 = readFile "input.txt" >>= print . refine . comp2 . getData

{-
*Main> exec2
([("row",[19])
,("zone",[8])
,("arrival location",[18])
,("train",[4])
,("departure time",[1])
,("departure platform",[13])
,("departure station",[14])
,("departure date",[11])
,("departure track",[9])
,("departure location",[2])
,("arrival platform",[12])
,("route",[16])
,("type",[3])
,("seat",[17])
,("class",[15])
,("wagon",[6])
,("arrival station",[10])
,("arrival track",[7])
,("duration",[0])
,("price",[5])]
,[])
いけた。

59,101,191,149,167,197,199,137,163,131,113,67,103,97,61,139,157,151,193,53

-}

comp3 (rss, tss) =  product [ head tss !! c | (n,[c]) <- ncsX, isPrefixOf "departure" n ]
  where
-- どの範囲にも合わない項目を持つ行を捨てる
    rs = concatMap snd rss
    isValid t = any (\(a,b) -> a <= t && t <= b) rs
    tss1 = filter (all isValid) tss
-- 欄ごとにデータを分け直す
    tcs = zip [0..] $ transpose tss1
-- 規則ごとに、何番目のカラムが全員合格かのリストを作る
    isOKwith rs tc = all (\t -> any (\(a,b) -> a <= t && t <= b) rs) tc
-- ここまでcomp2のコピペ
    ncs0 =
      [ (rn, map fst $ filter (isOKwith rs. snd) tcs)
      | (rn,rs) <- rss
      ]
-- 対応付けを取り出す
    (ncsX,[]) = refine ncs0

exec3 = readFile "input.txt" >>= print . comp3 . getData

{-
*Main> test1
[("class",[0,1]),("row",[0]),("seat",[2])]
*Main> test2
([("class",[1]),("row",[0]),("seat",[2])],[])
*Main> exec3
1001849322119
-}
