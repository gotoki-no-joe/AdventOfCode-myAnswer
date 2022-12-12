{-
Hの位置はコマンドで1歩ずつ動く。
Tの位置は、Hと隣接するように補正される。
それは、XとYのいずれかが2以上離れたときに、Hが元いた位置になる、という感じ。
どちらも差が1以内なら動かないだけ。
その位置の情報を集合に突っ込んで、要素数を数えよう。
-}
import qualified Data.Set as S
import Data.List
import Data.Array

import Control.Monad

-- まず、Hの位置の履歴を作る。

main1 = body1 "input.txt"
test1 = body1 "test.txt"

body1 fn = readFile fn >>= print . compute1 . lines

compute1 :: [String] -> Int
compute1 ls = S.size $ S.fromList tailPoss
  where
    headPoss =
      scanl add (0,0) $
      concatMap ((\[d,n] -> replicate (read n) (delta $ head d)) . words) ls
    tailPoss = scanl follow (0,0) $ zip headPoss (tail headPoss)

add (x,y) (z,w) = (x+z, y+w)

delta 'U' = (0,-1)
delta 'D' = (0, 1)
delta 'L' = (-1,0)
delta 'R' = ( 1,0)
delta _ = error "never"

-- 「ついていく」計算
follow (tx,ty) (hxy0,(hx,hy))
  | abs (tx - hx) > 1 || abs (ty - hy) > 1 = hxy0
  | otherwise = (tx,ty)

main2 = body2 "input.txt"
test2 = body2 "test.txt"
test3 = body2 "test2.txt"

body2 fn = readFile fn >>= print . compute2 . lines

-- compute2 :: [String] -> Int
compute2 ls = {-S.size $ -} S.fromList tailPoss
  where
    headPoss =
      scanl add (0,0) $
      concatMap ((\[d,n] -> replicate (read n) (delta $ head d)) . words) ls
    knot xys = scanl follow (0,0) $ zip xys (tail xys)
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

{-
どうもおかしいとおもったら、動きが理不尽だった。
パート1ではHは4近傍にしか動かないから、「どちらかに2離れたらさっきの位置に行く」で対応できたけど、
パート2では、一つ前は8近傍に移動できる。
でもそのときの動きが、サンプルで見て

......
....H.
4321.. から U で

....H.
.4321.
5..... になって、2～4の動きがあからさまにおかしい。1がURに動いて、どうして2がいっしょにURに動くのやら。

まぁ確かに、俺のやり方だと、斜めが増幅されないから問題としてつまらんけども、これだと不自然でそ。
というか、これはどう定義すればいいのやら？
「動かないときは8近傍までは離れても耐えるけど、移動するときは4近傍まで移動する」かな？
もっというと、「自分の8近傍の位置で、唯一の、先行と4近傍の位置に移動する」だろうか。

ver2.hsに移行。
-}
