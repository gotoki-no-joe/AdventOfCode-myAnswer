import qualified Data.IntMap as IM

{-
0は1になる
偶数桁の数は、数字で半分にする 1234 →12 と 34
さもなくば、2024倍される

25回やった後の石の個数
-}

sample :: [Int]
sample = [0,1,10,99,999]
sample2 :: [Int]
sample2 = [125,17]
input :: [Int]
input  = [510613,358,84,40702,4373582,2,0,1584]

step :: [Int] -> [Int]
step [] = []
step (0:xs) = 1 : step xs
step (x:xs)
  | even l = read a : read b : step xs
  | otherwise = x *2024 : step xs
  where
    s = show x
    l = length s
    (a,b) = splitAt (div l 2) s

main1 = length $ iterate step input !! 25

{-
単純に拡張しただけでは、時間がかかって仕方ない。
倍々を使いたくても、modもないのでどうしたものやら。
実行時メモ化のできるmutableな言語でやるしかないのか？

リストをリストでなく、どんな数がいくつあるか、というMapの形で持つのが正解だ。きっと。
-}

makeMap :: [Int] -> IM.IntMap Int
makeMap xs = IM.fromListWith (+) [(x,1) | x <- xs]

stepMap :: IM.IntMap Int -> IM.IntMap Int
stepMap xm = IM.fromListWith (+) [(j,v) | (k,v) <- IM.assocs xm, j <- step k]
  where
    step 0 = [1]
    step x
      | even l = [read a, read b]
      | otherwise = [x * 2024]
      where
        s = show x
        l = length s
        (a,b) = splitAt (div l 2) s

main1a = sum $ IM.elems $ (!! 25) $ iterate stepMap $ makeMap input

main2a = sum $ IM.elems $ (!! 75) $ iterate stepMap $ makeMap input

{-
ghci> main1a
217812
ghci> main2a
259112729857522

正解でした。一瞬で終わる。
-}
