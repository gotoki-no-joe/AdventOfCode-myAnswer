-- 2022-11-26
import Data.List

{-
input.txt の内容
To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.
-}

theRow = 2978 :: Int
theCol = 3083 :: Int

{-
斜めの列のK行めを考える。
それぞれの長さは順に1,2,3,...なので、その末尾の数は 1 + 2 + ... + K = K * (K + 1) / 2 となる。
またそれは行1列Kの位置にある。
行r列cのマスは、右上マスに(r-1)回移動した先にある、行1列(c+r-1)と同じ斜め行に属する。
つまり K = c + r - 1 行に属する。
そのひとつ手前、K-1行の最後のマスの番号は (K-1)*K/2 で、
行r列cのマスはそれからさらにc足したところにあるので (c+r-2)(c+r-1)/2 + c 番である。
-}

index r c = div (k * pred k) 2 + c
  where
    k = pred r + c

{-
1ステップの計算
-}

step :: Int -> Int
step x = mod (x * theMag) modBase

-- これをindex回行えばよいのだが。

theSeed = 20151125
theMag = 252533
modBase = 33554393

part1a = loop (pred $ index theRow theCol) theSeed
  where
    loop 0 x = x
    loop n x = loop (pred n) (step x)
-- こちらは止まらない

part1b = iterate step theSeed !! pred (index theRow theCol)
-- こちらは *** Exception: stack overflow でコケる。

{-
これはつまり、mod 33554393 で 20151125 * 252533 ^ pred index を求めろということなので、
べき乗を2進でやるアレをしろということ。
-}

-- @gotoki_no_joe
powerish mul i a b = foldl' {-'-} mul i [p | (True, p) <- zip bs ps]
  where
    bs = map odd $ takeWhile (0 <) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a

nth n = powerish mul theSeed theMag (pred n)

mul x y = mod (x * y) modBase

part1c = nth $ index theRow theCol
