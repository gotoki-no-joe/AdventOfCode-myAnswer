-- 2025/1/14 spoilerのためにやり直し

{-
ByteString使ったら速くなるかしら？急がないからいいけど。

よいabbaが見つかったかどうか、
カッコの深さ、
よくないabbaが見つかったかどうか
の3つの状態を持ってループする。
あと、abbaはカッコでは成立しないと思っていいんだろうか？

-}
import Debug.Trace
import Data.List

runner f = readFile "input.txt" >>= print . f. lines

part1 = length . filter isABBA

isABBA = loop False False 0
  where
    loop _ _ d [] | d > 0 = error "[[["
    loop _ _ d ('[':_) | d > 0 = error "[["
    loop _ _ d (']':_) | d < 1 = error "[["
    loop f1 f2 _ [] = f1 && not f2
    loop f1 f2 d ('[':xs) = loop f1 f2 (succ d) xs
    loop f1 f2 d (']':xs) = loop f1 f2 (pred d) xs
    loop f1 f2 d (x:y:z:w:xs) | x == w, y == z, x /= y = if d == 0 then loop True f2 d xs else loop f1 True d xs
    loop f1 f2 d (_:xs) = loop f1 f2 d xs

main1 = runner part1

{-
カッコの外なら ab, 中なら ba を記録し、等しいものがあるか調べる
深さは1と確認できたので、dはIntからBoolに変える
-}

part2 = length . filter isSSL

isSSL = loop [] [] False
  where
    loop abs bas _ [] = not $ null $ intersect abs bas
    loop abs bas f  ('[':xs) = loop abs bas True  xs
    loop abs bas f  (']':xs) = loop abs bas False xs
    loop abs bas f (x:xs@(y:z:_)) | x == z, x /= y = if f then loop abs ((y,x):bas) f xs else loop ((x,y):abs) bas f xs
    loop abs bas f (_:xs) = loop abs bas f xs

main2 = runner part2
