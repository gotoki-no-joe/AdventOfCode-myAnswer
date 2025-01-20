import Data.Char
import Data.Bits
import Data.Array.Unboxed
import Control.Monad
import Data.List.Split
import Data.List

-- infrate sz xs : xs を元に長さsz以上のドラゴン曲線を作る
infrate :: Int -> [Int] -> [Int]
infrate sz xs0 = loop (length xs0) xs0
  where
    loop len xs
      | sz <= len = xs
      | otherwise = loop (len + succ len) (xs ++ 0 : map (1 -) (reverse xs))

-- defrate sz xs : xsの前からszをチェックサムにする
defrate :: Int -> [Int] -> [Int]
defrate sz xs
  | even sz   = defrate (div sz 2) (checksum xs)
  | otherwise = take sz xs
  where
    checksum (a:b:as) = (if a == b then 1 else 0) : checksum as
    checksum _ = []

solve1 :: String -> Int -> String
solve1 cs sz = map intToDigit . defrate sz . infrate sz . map digitToInt $ cs

main1 = solve1 "00111101111101000" 272
main2 = solve1 "00111101111101000" 35651584

data Dragon
  = Str                      -- 与えられた数字列による列
  | Triple Dragon Int Dragon -- 前半の反転を後ろに繋いで倍にした列
  | Rev Dragon               -- 逆順にし、01を反転させた列

infrate2 :: Int -> [Int] -> [Int]
infrate2 sz xs = get dragon []
  where
    sx = map (1 -) (reverse xs)
    dragon = loop (length xs) Str
    loop len d
      | sz <= len = d
      | otherwise = loop (len + succ len) (Triple d 0 (Rev d))
    get Str                  rest = xs ++ rest
    get (Triple a b c)       rest = get a $ b : get c rest
    get (Rev (Rev d))        rest = get d rest
    get (Rev  Str)           rest = sx ++ rest
    get (Rev (Triple a b c)) rest = get (Rev c) $ (1 - b) : get (Rev a) rest

defrate2 :: Int -> [Int] -> [Int]
defrate2 sz xs
  | r == 0    = defrate2 q          (shortcut xs)
  | even sz   = defrate2 (div sz 2) (checksum xs)
  | otherwise = take sz xs
  where
    (q,r) = divMod sz 16
    checksum (a:b:as) = (if a == b then 1 else 0) : checksum as
    checksum _ = []
    table = listArray (0, 65535)
      [ head $ checksum $ checksum $ checksum $ checksum bs
      | bs <- replicateM 16 [0,1]] :: UArray Int Int
    shortcut [] = []
    shortcut xs = table ! sum (zipWith shiftL xs [0 .. 15]) : shortcut (drop 16 xs)

solve2 cs sz = map intToDigit . defrate2 sz . infrate2 sz . map digitToInt $ cs

main1a = solve2 "00111101111101000" 272
main2a = solve2 "00111101111101000" 35651584

defrate3 :: Int -> [Int] -> [Int]
defrate3 sz xs = take cslen $ map ((1 .^.) . (1 .&.) . sum) $ chunksOf len xs
  where
    len = sz .^. pred sz
    cslen = div sz len

solve3 cs sz = map intToDigit . defrate3 sz . infrate2 sz . map digitToInt $ cs

main1b = solve2 "00111101111101000" 272
main2b = solve2 "00111101111101000" 35651584

{-
ghci> main1b
"10011010010010010"
(0.64 secs, 351,996,872 bytes)
ghci> main2b
"10101011110100011"
(14.54 secs, 14,118,332,760 bytes)
-}
