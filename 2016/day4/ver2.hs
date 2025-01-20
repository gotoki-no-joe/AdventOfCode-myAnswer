-- 2025/1/14 for spoiler

import Data.List.Split
import Data.Array
import Data.List
import Data.Tuple
import Data.Char

runner i f = readFile i >>= print . f . map parse . lines

data Entry = Entry {names :: [String], sect :: Int, checkSum :: String}

parse :: String -> Entry
parse l = Entry { names = as, sect = read sect, checkSum = check }
  where
    ws = endByOneOf "-[]" l
    (as,[sect, check]) = splitAt (length ws - 2) ws

-- part1

part1 :: [Entry] -> Int
part1 = sum . map sect . filter valid

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

-- 文字の頻度を数える
-- 頻度の降順、文字の昇順で前から5つ取り出すとチェックサムになる
-- 元のものと等しいか判定する
valid :: Entry -> Bool
valid e = checkSum e == check1
  where
    cnt :: Array Char Int
    cnt = accumArray (+) 0 ('a','z') [(x, -1) | x <- concat $ names e]
    check1 = take 5 . map snd . sort . map swap . filter ((0 /=) . snd) $ assocs cnt

-- part2

{-
シーザー暗号だそれ
文字を0から25に置き換え、部屋番号を足してmod 26して、元に戻す
Data.Char の ord と chr を使うのが正しいのかな。
Arrayで勝手に表を作るという手もあるけど。

ghci> runner "input.txt" (length . filter valid)
749
こんなにあるんだけど。
-}

part2 :: [Entry] -> [(String, Int)]
part2 = map decode . filter valid

decode e = (unwords $ map (map (i2a . (sect e +) . a2i)) $ names e, sect e)
  where
    orda = ord 'a'
    a2i c = ord c - orda
    i2a i = chr $ mod i 26 + orda

main2 = runner "input.txt" part2

{-
ghci> decode $ parse "qzmt-zixmtkozy-ivhz-343[]"
("very encrypted name",343)
ghci> main2
[("top secret chocolate management",377),("top secret cryogenic plastic grass shipping",422)
,("international bunny sales",227),...

何か全部意味のある言葉になっている。エディタでnorthを検索するとあるので、それを見つけるように変更
-}

findNorth (a,_) = isInfixOf "north" a

-- main2a = runner "input.txt" (filter findNorth . part2)

part2a q = filter (isInfixOf q . fst) . map decode . filter valid

main2a q = runner "input.txt" (part2a q)
