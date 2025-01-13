-- 2025-1-12
import Data.Array
import Data.List

runner i f = readFile i >>= print . f . map read . lines

-- 上と下で枝刈りする
part1a goal cups = iter goal cups (sum cups)
  where
    iter 0 _ _ = 1
    iter rest _ _ | rest < 0 = 0
    iter rest _ remain | rest > remain = 0
    iter _ [] _ = 0
    iter rest (x:xs) remain = iter rest xs remain1 + iter (rest - x) xs remain1
      where
        remain1 = remain - x

test1a = runner "test.txt" (part1a 25)
main1a = runner "input.txt" (part1a 150)

-- 枝刈りなしのテスト版
part1b goal cups = iter 0 cups
  where
    iter acc [] = if acc == goal then 1 else 0
    iter acc (x:xs) = iter acc xs + iter (acc + x) xs

test1b = runner "test.txt" (part1b 25)
main1b = runner "input.txt" (part1b 150)

-- 少し時間かかるといえばかかるけど、答え出ちゃった。

part1c goal cups = arrN ! goal
  where
    arr0 = listArray (0, goal) $ 1 : repeat 0
    arrN = foldl' step arr0 cups
    step arr a = accum (+) arr $ zip [a .. goal] $ elems arr

test1c = runner "test.txt" (part1c 25)
main1c = runner "input.txt" (part1c 150)

-- パート2

-- 全探索版

part2a goal cups = (head ks, length ks) -- 使う容器の個数とその場合の数
  where
    iter acc cnt [] ans = if acc == goal then cnt : ans else ans
    iter acc cnt (x:xs) ans = iter acc cnt xs $ iter (acc + x) (succ cnt) xs ans
    ks = head $ group $ sort $ iter 0 0 cups []

test2a = runner "test.txt" (part2a 25)
main2a = runner "input.txt" (part2a 150)

part2b goal cups = head $ filter ((0 <) . snd) $ assocs cnts
  where
    arr0 = listArray (0, goal) $ [0] : repeat []
    arrN = foldl' step arr0 cups
    step arr a = accum (++) arr $ zip [a .. goal] $ map (map succ) $ elems arr
    cnts = accumArray (+) 0 (0, length cups)
           [(k,1) | k <- arrN ! goal]

test2b = runner "test.txt" (part2b 25)
main2b = runner "input.txt" (part2b 150)
