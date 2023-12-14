import Data.List

import Data.Array.IO
import qualified Data.Map as M
import Control.Monad

part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve1 ls

solve1 ls =
    -- unlines $ map (concatMap f . groupBy gf) $ transpose ls
     sum $ map (value . concatMap f . groupBy gf) $ transpose ls
  where
    isSol c = c == '#'
    gf a b = isSol a == isSol b
    f xs@('#':_) = xs
    f xs = let (as,bs) = partition ('.' ==) xs in bs ++ as
    vf v 'O' = v
    vf _ _ = 0
    value xs = sum $ zipWith vf [1..] $ reverse xs

{-
列ごとに考えて総和をとる。

ghci> part1 "sample.txt"
136
ghci> part1 "input.txt"
108955

パート2 向きを変えて10^9回繰り返せと。どうやってループ発見するか？
まず、mutable array上の操作に変更しよう。
そして、#を無視してOを順に1として、Integerに直して、Mapで普通に記憶しよう。
それで、ループが見つかったところで出力するところまで。
-}

part2 fn = readFile fn >>= solve2 . lines

solve2 ls =
  do
    arr <- newListArray ((1,1),(h,w)) $ concat ls :: IO (IOArray (Int,Int) Char)
    res1@(cnt1, cnt0) <- loop arr 0 (M.empty :: M.Map Integer Int)
    print res1
    let n = mod (1_000_000_000 - cnt0) (cnt1 - cnt0)
    forM_ [1..n] (\_ -> tilt4 arr)
    score arr >>= print

  where
    w = length (head ls)
    h = length ls
    xys = [(x,y) | (x,l) <- zip [1..] ls, (y,c) <- zip [1..] l, c /= '#']
    loop arr cnt m = do
--      showArr arr
      -- arrを番号に変換
      i <- foldM (\acc xy -> do
        c <- readArray arr xy
        return (acc * 2 + if c == 'O' then 1 else 0)
        ) 0 xys
      if M.member i m then return (cnt, m M.! i) else do
        let m1 = M.insert i cnt m
        -- 4 方向にズラす
        tilt4 arr
        -- 繰り返し
        loop arr (succ cnt) m1
    showArr arr =
      forM_ [1..h] (\i -> do
        forM_ [1..w] (\j -> readArray arr (i,j) >>= putChar)
        putChar '\n'
        )
      >> putChar '\n'
    -- 4 方向にズラす
    tilt4 arr = do
        forM_ [1..w] (\j -> do -- N
          l <- tiltLine <$> forM [1..h] (\i -> readArray arr (i,j))
          forM_ (zip [1..h] l) (\(i, c) -> writeArray arr (i,j) c)
          )
        forM_ [1..h] (\i -> do -- W
          l <- tiltLine <$> forM [1..w] (\j -> readArray arr (i,j))
          forM_ (zip [1..w] l) (\(j,c) -> writeArray arr (i,j) c)
          )
        forM_ [1..w] (\j -> do -- S
          l <- tiltLine <$> forM [h,pred h..1] (\i -> readArray arr (i,j))
          forM_ (zip [h, pred h..1] l) (\(i,c) -> writeArray arr (i,j) c)
          )
        forM_ [1..h] (\i -> do -- E
          l <- tiltLine <$> forM [w, pred w..1] (\j -> readArray arr (i,j))
          forM_ (zip [w, pred w..1] l) (\(j,c) -> writeArray arr (i,j) c)
          )
-- スコア計算
    score arr =
      sum <$> forM [1..h] (\i ->
        ((h - pred i) *) . sum <$> forM [1..w] (\j -> do
          c <- readArray arr (i,j)
          return $ if c == 'O' then 1 else 0
          ))

tiltLine xs = loop 0 0 xs
  where
    loop cO cd ('.':xs) = loop cO (succ cd) xs
    loop cO cd ('O':xs) = loop (succ cO) cd xs
    loop cO cd xs = replicate cO 'O' ++ replicate cd '.' ++ if null xs then [] else '#' : loop 0 0 (tail xs)

{-
3サイクル後と10サイクル後が同じということは、10^9サイクル後とは、
(10^9 - 3) `mod` (10 - 3)
だけ、現状からもう一度進めたところになる。

ghci> part2 "sample.txt"
(10,3)
64
ghci> part2 "input.txt"
(107,85)
106689

一発正解は気持ちいいね。
-}
