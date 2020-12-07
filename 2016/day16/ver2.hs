import Data.Char

encode :: String -> [Int]
encode = map digitToInt

decode :: [Int] -> String
decode = map intToDigit

{-
もっと高速にしたいわ。
再帰呼び出しの底で(++)とか使っていると大変。
今まで出力したものを全て逆順に保存しておいて、
それを使いつくしたら、0はさんでそれを反転したものを出力する、という感じ。
-}

stream xs = result
  where
    result = loop1 0 xs
    loop1 c (x:xs) = x : loop1 (succ c) xs
    loop1 c [] = 0 : loop2 (succ c) (map (1 -) $ reverse $ take c result)
    loop2 c (y:ys) = y : loop2 (succ c) ys
    loop2 c [] = 0 : loop2 (succ c) (map (1 -) $ reverse $ take c result)

test1 = decode $ take 20 $ stream $ encode "10000"

-- あとは同じで

checksum :: [Int] -> [Int]
checksum (x:y:xs) = (if x == y then 1 else 0) : checksum xs
checksum _ = []

check :: [Int] -> Int -> [Int]
check is size = fst $ head $ dropWhile (even.snd) $ iterate enchecksum (is, size)

enchecksum (is, size) = (checksum is, div size 2)

compute s sz = decode $ check (take sz $ stream $ encode s) sz

test2 = compute "10000" 20

ans1 = compute "00111101111101000" 272

ans2 = compute "00111101111101000" 35651584

{- やっぱり大変。-}
