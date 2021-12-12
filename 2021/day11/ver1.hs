import Data.Array.IO
import Data.Char
import Data.Array
import Control.Monad
import Control.Applicative

test1 = readFile "sample.txt" >>= compute1 >>= print

run1 = readFile "input.txt" >>= compute1 >>= print

{-
これのモデル化はなんか難しいな。
1. 全てを+1する
2. 9を超えたものが発火し、周囲を+1する、これは発火しなくなるまで連鎖する
3. 発火したものは全て0になる

mutable vectorでやるべきかな。発火したものは-100とかにしておいて、
負のやつを全て0に戻すとき、その数を数えたら発火数がわかる。

結果がおかしいのは、連鎖を忘れていたからか。難しいなおい。
-}

type OctoMap = IOArray (Int,Int) Int

compute1 :: String -> IO Int
compute1 xs =
  do
    arr <- thaw a0
    sum <$> forM [1..100] (\_ -> step arr)
  where
    ls = lines xs
    (w,h) = (length (head ls), length ls)
    a0 = array ((1,1),(w,h)) [((i,j), digitToInt c) | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l]
    ps = [(i,j) | i <- [1..w], j <- [1..h]]
    adjs (i,j) = [(x,y) | x <- [pred i | i > 1] ++ i : [succ i | i < w]
                        , y <- [pred j | j > 1] ++ j : [succ j | j < h], x /= i || y /= j]

    step :: OctoMap -> IO Int
    step arr = do
    -- 全て1増やす
      forM_ ps (\p -> do
        v <- readArray arr p
        writeArray arr p (succ v)
        )
    -- 発火しなくなるまで全体を発火させる
      ignite arr
    -- 発火した点を0リセットして、その個数を返す
      length . filter id <$> forM ps (\p -> do
        v <- readArray arr p
        if v >= 0 then return False else do
          writeArray arr p 0
          return True
        )

    ignite :: OctoMap -> IO ()
    ignite arr = do
      f <- or <$> forM ps (\p -> do
        d <- readArray arr p
        if d <= 9 then return False else do
          writeArray arr p (-100)
          forM_ (adjs p) (\q -> do
            e <- readArray arr q
            writeArray arr q (succ e)
            )
          return True
        )
      when f $ ignite arr

-- part 2
-- 部品化が微妙で再利用できないぞ。てへ。

test2 = readFile "sample.txt" >>= compute2 >>= print

run2 = readFile "input.txt" >>= compute2 >>= print

compute2 :: String -> IO Int
compute2 xs =
  do
    arr <- thaw a0
    countloop arr 0
  where
    ls = lines xs
    (w,h) = (length (head ls), length ls)
    wh = w * h
    a0 = array ((1,1),(w,h)) [((i,j), digitToInt c) | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l]
    ps = [(i,j) | i <- [1..w], j <- [1..h]]
    adjs (i,j) = [(x,y) | x <- [pred i | i > 1] ++ i : [succ i | i < w]
                        , y <- [pred j | j > 1] ++ j : [succ j | j < h], x /= i || y /= j]

    countloop :: OctoMap -> Int -> IO Int
    countloop arr cnt = do
      let cnt1 = succ cnt
      c <- step arr
      if c == wh then return cnt1 else countloop arr cnt1

    step :: OctoMap -> IO Int
    step arr = do
    -- 全て1増やす
      forM_ ps (\p -> do
        v <- readArray arr p
        writeArray arr p (succ v)
        )
    -- 発火しなくなるまで全体を発火させる
      ignite arr
    -- 発火した点を0リセットして、その個数を返す
      length . filter id <$> forM ps (\p -> do
        v <- readArray arr p
        if v >= 0 then return False else do
          writeArray arr p 0
          return True
        )

    ignite :: OctoMap -> IO ()
    ignite arr = do
      f <- or <$> forM ps (\p -> do
        d <- readArray arr p
        if d <= 9 then return False else do
          writeArray arr p (-100)
          forM_ (adjs p) (\q -> do
            e <- readArray arr q
            writeArray arr q (succ e)
            )
          return True
        )
      when f $ ignite arr

{-
compute1の部品を再利用できるように、
igniteやstepが局所変数を覗くスタイルを崩さずに外部定義にできるように、
するコーディングスタイルが見つからない。
-}
