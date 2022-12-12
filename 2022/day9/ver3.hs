{-
2022-12-12
問題をちゃんと読んだら、動き方はちゃんと定義が書いてあったのでそれでやり直し。
-}
import qualified Data.Set as S
import Control.Monad

-- 追尾動作

follow (tx,ty) (hx,hy)
  | abs dx <= 1 && abs dy <= 1 = (tx,ty)
  | dx == 0 = (tx, ty1)
  | dy == 0 = (tx1, ty)
  | otherwise = (tx1, ty1)
  where
    dx = hx - tx
    dy = hy - ty
    tx1 = tx + signum dx
    ty1 = ty + signum dy

-- ファイルを読み込み、頭の位置の系列を作る
-- 尾を追尾させ、その座標を集合に入れて、要素数を数える。

main1 = body1 "input.txt"
test1 = body1 "test.txt"

body1 fn = readFile fn >>= print . compute1 . lines

compute1 :: [String] -> Int
compute1 ls = S.size $ S.fromList tailPoss
  where
    headPoss =
      scanl add (0,0) $
      concatMap ((\[d,n] -> replicate (read n) (delta $ head d)) . words) ls
    tailPoss = scanl follow (0,0) headPoss

add (x,y) (z,w) = (x+z, y+w)

delta 'U' = (0,-1)
delta 'D' = (0, 1)
delta 'L' = (-1,0)
delta 'R' = ( 1,0)
delta _ = error "never"

--- part2

main2 = body2 "input.txt"
test2 = body2 "test.txt"
test3 = body2 "test2.txt"

body2 fn = readFile fn >>= print . S.size . compute2 . lines

-- compute2 :: [String] -> Int
compute2 ls = {- S.size $ -} S.fromList tailPoss
  where
    headPoss =
      scanl add (0,0) $
      concatMap ((\[d,n] -> replicate (read n) (delta $ head d)) . words) ls
    knot xys = scanl follow (0,0) xys
    tailPoss = iterate knot headPoss !! 9

visualize s =
  forM_ [yl..yh] (\y -> do
    forM_ [xl..xh] (\x -> do
      putChar $ if S.member (x,y) s then '#' else '.'
      )
    putChar '\n'
    )
  where
    xys = S.elems s
    xl = minimum $ map fst xys
    xh = maximum $ map fst xys
    yl = minimum $ map snd xys
    yh = maximum $ map snd xys

test4 = readFile "test2.txt" >>= visualize . compute2 . lines
