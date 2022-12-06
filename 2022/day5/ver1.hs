-- 2202-12-5
import Data.List
import qualified Data.IntMap as IM

-- 最初のパートは、場所を決め打ちして読み込む。行の長さが全て同じで助かった。

main = do
  (ls1, _:ls2) <- break null . lines <$> readFile "input.txt"
  let ls1t = transpose $ init ls1
  let m0 = IM.fromAscList $ zip [1..] $ map (dropWhile (' ' ==) . (ls1t !!)) [1,5..33]
  let mZ = foldl step1 m0 $ map parse ls2
  putStrLn $ map head $ IM.elems mZ
  let mY = foldl step2 m0 $ map parse ls2
  putStrLn $ map head $ IM.elems mY

parse :: String -> (Int,Int,Int)
parse xs = (read w1, read w2, read w3)
  where
    (_:w1:_:w2:_:w3:_) = words xs

step1 :: IM.IntMap String -> (Int,Int,Int) -> IM.IntMap String
step1 m1 (a,b,c) = IM.insert c imc $ IM.insert b imb m1
  where
    move 0 xs ys = (xs,ys)
    move k (x:xs) ys = move (pred k) xs (x:ys)
    (imb, imc) = move a (m1 IM.! b) (m1 IM.! c)

step2 :: IM.IntMap String -> (Int,Int,Int) -> IM.IntMap String
step2 m1 (a,b,c) = IM.insert c imc $ IM.insert b imb2 m1
  where
    (imb1, imb2) = splitAt a (m1 IM.! b)
    imc = imb1 ++ m1 IM.! c

body fn step =
  do
    (ls1, _:ls2) <- break null . lines <$> readFile fn
    let ls1t = transpose $ init ls1
    let m0 = IM.fromAscList $ zip [1..] $ map (dropWhile (' ' ==) . (ls1t !!)) [1,5..33]
    let mZ = foldl step m0 $ map parse ls2
    putStrLn $ map head $ IM.elems mZ

main2 = body "input.txt" step2
main1 = body "input.txt" step1
