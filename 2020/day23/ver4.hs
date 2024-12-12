import Data.Array.IO
import Control.Monad
import Data.Char

sample = "389125467"
input = "784235916"

part1 ds =
  do
    let xs = map digitToInt ds
-- 数 x の次に並ぶ数を持つ。リンクリストをなす
    arr <- newArray_ (1,9) :: IO (IOUArray Int Int)
-- 初期リンクを設定
    forM_ (zip (last xs : xs) xs) (uncurry (writeArray arr))
-- 100回moveをする
    foldM_ (\current _ -> move arr current) (head xs) [1 .. 100]
-- 答えを出力
    readArray arr 1 >>= genAns arr >>= putStrLn
  where
-- 答えを生成する
    genAns _rr 1 = return ""
    genAns arr p = do
      q <- readArray arr p
      res <- genAns arr q
      return $ intToDigit p : res

predd (lb, ub) x
  | x == lb   = ub
  | otherwise = pred x

-- 操作をする
move arr current = do
-- currentの次3つの数を掴む
  three1 <- readArray arr current
  three2 <- readArray arr three1
  three3 <- readArray arr three2
  nextofC <- readArray arr three3
-- destinationはcurrentの前の値、ただしthree*とは異なる最大のもの
  bnds <- getBounds arr
  let isOK x = notElem x [three1, three2, three3]
  let dest = head $ filter isOK $ tail $ iterate (predd bnds) current
  nextofD <- readArray arr dest
-- 接続を変更する
  writeArray arr current nextofC
  writeArray arr dest three1
  writeArray arr three3 nextofD
  return nextofC

{-
ghci> part1 sample
67384529
ghci> part1 input
53248976

ふふ、もうできたも同然だ！
-}

million = 1000000

part2 ds =
  do
--    let xs = [base + d | base <- [0, 9 .. million - 10], d <- map digitToInt ds] ++ [million]
-- 違うわこれ！
    let xs = map digitToInt ds ++ [10 .. million]
-- 数 x の次に並ぶ数を持つ。リンクリストをなす
    arr <- newArray_ (1,million) :: IO (IOUArray Int Int)
-- 初期リンクを設定
    forM_ (zip (million : xs) xs) (uncurry (writeArray arr))
-- ten million 回moveをする
    foldM_ (\current t -> move arr current) (head xs) [1 .. 10 * million]
-- 答えを出力
    ans1 <- readArray arr 1
    ans2 <- readArray arr ans1
    print (ans1, ans2, ans1 * ans2)

main = do
  part2 sample
  part2 input

{-
> ./ver4
(934001,159792,149245887792)
(635429,659113,418819514477)
よっしゃ！
コンパイルしないとだけど、コンパイルしたらスグ終わる。
-}
