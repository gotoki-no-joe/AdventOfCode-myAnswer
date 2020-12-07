import Data.Char

encode :: String -> [Int]
encode = map digitToInt

decode :: [Int] -> String
decode = map intToDigit

{- 一度延ばす操作 -}
{- 長さは 2n+1 に増える -}

dragon :: [Int] -> [Int]
dragon xs = xs ++ 0 : reverse (map (1 -) xs)

build :: [Int] -> Int -> [Int]
build is size = take size $ fst $ head $ dropWhile ((size >).snd) $ iterate endragon (is, length is)

endragon (is,len) = (dragon is, len + len + 1)

{- checksum -}
{- 長さは div 2 に減る -}

checksum :: [Int] -> [Int]
checksum (x:y:xs) = (if x == y then 1 else 0) : checksum xs
checksum _ = []

check :: [Int] -> Int -> [Int]
check is size = fst $ head $ dropWhile (even.snd) $ iterate enchecksum (is, size)

enchecksum (is, size) = (checksum is, div size 2)

phase1 s sz = decode $ check (build (encode s) sz) sz

test1 = phase1 "10000" 20

ans1 = phase1 "00111101111101000" 272

ans2 = phase1 "00111101111101000" 35651584

{-
*Main> test1
"01100"
*Main> ans1
"10011010010010010"
*Main> ans2
"10101011110100011"

忠実に計算するとめちゃ遅い。iterateで無限リスト作っているのでStrictも効かない。
-}
