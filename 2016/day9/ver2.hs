-- 2025/1/15 for spoiler
import Data.List

runner f = readFile "input.txt" >>= print . f . head . lines

{-
`(`、数字列、`x`、数字列、`)` と来たらそのようにして、そうでないときは1文字ずつ消費
バイト数だけ数えたらいいのだけど、伸張した結果そのものを作ってみる。
-}

decompress1 l = loop l
  where
    loop ('(':xs) = concat (replicate (read xs3) xs5) ++ loop xs6
      where
        (xs1, _:xs2) = break ('x' ==) xs
        (xs3, _:xs4) = break (')' ==) xs2
        (xs5, xs6) = splitAt (read xs1) xs4
    loop (x:xs) = x : loop xs
    loop [] = []

part1 = length . decompress1

main1 = runner part1

{-
長さだけ数えるには、伸張タグが生成する文字数と、その他の文字数を数えていくだけ。
-}

decompress1len l = loop 0 l
  where
    loop !acc ('(':xs) = loop (acc + read xs1 * read xs3) xs6
      where
        (xs1, _:xs2) = break ('x' ==) xs
        (xs3, _:xs4) = break (')' ==) xs2
        xs6          = drop (read xs1) xs4
    loop !acc (x:xs) = loop (succ acc) xs
    loop acc [] = acc

part1a = decompress1len

main1a = runner part1a

decompress1len1 l = loop l
  where
    loop ('(':xs) = read xs1 * read xs3 + loop xs6
      where
        (xs1, _:xs2) = break ('x' ==) xs
        (xs3, _:xs4) = break (')' ==) xs2
        xs6          = drop (read xs1) xs4
    loop (x:xs) = succ $ loop xs
    loop [] = 0

part1b = decompress1len1

main1b = runner part1b

{-
区間を取り出して、それを再帰的に長さ数えて、結果を繰り返し回数倍して、とやる。
takeでリストのコピーが作られるのが気になるけど、ByteStringにするのも面倒だしな。
-}

decompress2len l = loop 0 l
  where
    loop !acc ('(':xs) = loop (acc + loop 0 xs5 * read xs3) xs6
      where
        (xs1, _:xs2) = break ('x' ==) xs
        (xs3, _:xs4) = break (')' ==) xs2
        (xs5, xs6)   = splitAt (read xs1) xs4
    loop !acc (x:xs) = loop (succ acc) xs
    loop acc [] = acc

part2 = decompress2len

main2 = runner part2

decompress2len1 l = loop l
  where
    loop ('(':xs) = loop xs5 * read xs3 + loop xs6
      where
        (xs1, _:xs2) = break ('x' ==) xs
        (xs3, _:xs4) = break (')' ==) xs2
        (xs5, xs6)   = splitAt (read xs1) xs4
    loop (x:xs) = succ $ loop xs
    loop [] = 0

part2b = decompress2len1

main2b = runner part2b
