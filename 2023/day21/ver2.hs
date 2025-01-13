{-
何となくわかった。できるか保証はないけど、やれそうな方法。

マップのあらゆる区画について、
- そこから1歩歩いた先とは、1歩歩いた先の最大4区画。
- そこから2歩歩いた先とは、1ほ歩いた先の全てのについてもう1歩歩いた先。
で、厳密に座標を調べるやり方で、powerishをするのではないか？
繰り返すマップの向こうで展開する座標はちゃんと展開して使って。

-}

import Data.Array.Unboxed
import qualified Data.Set as S
import Data.List

type POS = (Int,Int)

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((0,0),(pred h, pred w)) $ concat ls :: UArray POS Char
  print $ f fld

test1 = runner "sample.txt" $ part1 6
main1 = runner "input.txt" $ part1 64

part1 :: Int -> UArray POS Char -> Int
part1 steps fld = S.size $ ar ! spos
  where
    spos = head [s | (s, 'S') <- assocs fld]
    fldb = amap ('#' /=) fld
    bnds = bounds fld
    i, onestep :: Array POS (S.Set POS)
    i = listArray bnds [if b then S.singleton i else S.empty | (i,b) <- assocs fldb]
    onestep = listArray bnds
        [ if c then S.fromList [j | j <-[(pred a,b),(succ a,b),(a, pred b),(a, succ b)], inRange bnds j, fldb ! j ] else S.empty
        | ((a,b),c) <- assocs fldb]
    op ar bs = listArray bnds [S.unions [bs ! c | c <- S.elems b] | (a,b) <- assocs ar]
    ar = powerish op i onestep steps

-- @gotoki_no_joe
powerish mul i a b = foldl' mul i [p | (True, p) <- zip bs ps]
  where
    bs = map odd $ takeWhile (0 <) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a

{-
ghci> test1
16
ghci> main1
3660

おk

マップはよく見ると、wrap-aroundしたときに壁に突っ込まないように間を空けてあるやさしさ。
ちと計算量はあるが、これなら何とかなるでしょう。
-}

test2 = mapM_ (runner "sample.txt" . part2) [6,10,50,100,500,1000,5000]
main2 = runner "input.txt" $ part2 26501365

part2 :: Int -> UArray POS Char -> Int
part2 steps fld = S.size $ ar ! spos
  where
    spos = head [s | (s, 'S') <- assocs fld]
    fldb = amap ('#' /=) fld
    bnds@(_,(p1,q1)) = bounds fld
    p = succ p1
    q = succ q1
    i, onestep :: Array POS (S.Set POS)
    i = listArray bnds [if b then S.singleton i else S.empty | (i,b) <- assocs fldb]
    onestep = listArray bnds
        [ if c then S.fromList [j | j <-[(pred a,b),(succ a,b),(a, pred b),(a, succ b)], check j ] else S.empty
        | ((a,b),c) <- assocs fldb]
-- arの各位置aに関して、移動先集合bはbndsの外にも出る。
-- (p,q)で割った余りで配列をアクセスして、それに商を足してオフセットを戻す
-- という操作が必要
    op ar bs = listArray bnds
        [ S.unions [S.map (shift i k) $ bs ! (j,l) | (x,y) <- S.elems b, let (i,j) = divMod x p, let (k,l) = divMod y q]
        | (a,b) <- assocs ar]
    ar = powerish op i onestep steps
    check (a,b) = fldb ! (c,d)
      where
        c = mod a p
        d = mod b q
    shift i k (j,l) = (i * p + j, k * q + l)

main = main2

{-
ghci> test2
16
50
1594
6536
インタプリタだとこの辺で辛くなったので、コンパイル実行に切り替える。

しかし33ビットの値なので倍々でも32枚のレイヤーを通過して、
例でも100から1000で10倍ステップで100倍になっているので、
直接数えるようなスタイルでは答えを出すのは厳しい感じか。
33ビットのマンハッタン距離を移動できるから、^2の座標範囲に分布している訳だし。

…じゃあどうすんだ？
-}
