import Data.Char

encode :: String -> [Int]
encode = map digitToInt

decode :: [Int] -> String
decode = map intToDigit

{-
出力したものを、反転逆転してリストに蓄積する。
出力するものが尽きたら、それを出力していくが、それもリストに蓄積する、でどうだろう。
-}

stream xs = loop xs []
  where
    loop (x:xs) acc = x : loop xs ((1-x):acc)
    loop [] acc = 0 : loop acc (1:acc)

test1 = decode $ take 20 $ stream $ encode "10000"

{-
あとは同じ...では芸がない。
2つを1つにする計算が定義されている。
問題の35651584 = 17 * 2^21 なので、その計算を21回も繰り返している。
何段かを一度にやると効率がいい。
k段を一度にすると、2^k個を1つにする計算となる。3*7=21だが、2^7=128ビットはつらい。
2^3=8はまぁまぁか。端数が出るが2^4=16だと、65536通り。どうだろう。
きっとこれで効率は改善されるが、00..00～11..11を生成してテーブル作るコードを書くのが面倒だから
また思い出したらやるということで。
-}

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
