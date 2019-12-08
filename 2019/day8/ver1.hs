import Data.List.Split
import Data.List

main = do
    co0 <- readFile "input.txt"
    let co = init co0
    let ans = compute co
    print ans

compute :: String -> Int
compute co = snd $ minimum ns
  where
    ls = chunksOf (25*6) co
    count c = length . filter (c ==)
    ns = [ (count '0' l, count '1' l * count '2' l) | l <- ls ]

{-
*Main> main
1064
-}

main2 = do
    co0 <- readFile "input.txt"
    let co = init co0
    let ans = compute2 co
    putStrLn ans

compute2 :: String -> String
compute2 co = unlines $ chunksOf 25 $ map d2p $ map (foldl1 op) $ transpose $ chunksOf (25*6) co

-- 重ね合わせ演算
op '2' y = y
op  x  _ = x

-- 表示変換
d2p '0' = '　'
d2p '1' = '■'

{-
*Main> main2
■■■　　■■■■　　■■　　　■■　　■　　■　
■　　■　■　　　　■　　■　■　　■　■　■　　
■　　■　■■■　　■　　　　■　　■　■■　　　
■■■　　■　　　　■　　　　■■■■　■　■　　
■　　　　■　　　　■　　■　■　　■　■　■　　
■　　　　■　　　　　■■　　■　　■　■　　■　
-}