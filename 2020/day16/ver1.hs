{-
これは、この奇妙なファイル形式を読み込むことがまず一つのチャレンジになっていると理解するべきなのだろうか。
ruleは全て
(name): %d-%d or %d-%d
の形式で、orはひとつだけかならず、という感じ。なので区間はちょうど二つだけど、リストで扱うべきかね。
-}

import Data.Char

sample = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"

parseRule :: String -> [(Int,Int)]
parseRule line = [(read xs2,read xs4),(read xs6,read $ tail xs7)]
  where
    xs1 = drop 2 $ dropWhile (':' /=) line
    (xs2,xs3) = span isDigit xs1
    (xs4,xs5) = span isDigit (tail xs3)
    (xs6,xs7) = span isDigit $ dropWhile (not.isDigit) xs5

parseTicket :: String -> [Int]
parseTicket line = read ('[': line ++ "]")

getData co = (map parseRule rules, map parseTicket $ drop 5 ls1)
  where
    (rules,ls1) = span (not.null) $ lines co

comp1 (ruless, ticketss) = filter (not.isValid) tickets
  where
    rules = concat ruless
    tickets = concat ticketss
    isValid t = any (\(a,b) -> a <= t && t <= b) rules

ans1 = readFile "input.txt" >>= print . sum . comp1 . getData

{-
機械的にするにはどうしたらいいんだ。
まず、ticketから、ひとつでもinvalidなものがある行を全て捨てる。
次に、ticketをtransposeして、フィールドごとに分ける。自分のも入れる？
そして、ruleのそれぞれに対して、全員がそのフィールドの条件を満たすかを判定して、matrixを作る。

めんどくせーな。
-}
