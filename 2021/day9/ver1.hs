import Data.Char
import Control.Applicative
import Data.Array

import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Data.List

compute1 :: String -> Int
compute1 xs = length lps + sum lps
  where
    ls = lines xs
    h = length ls
    w = length (head ls)
    arr = array ((0,0),(succ w, succ h)) $
          [((i,j), digitToInt c) | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l] ++
          [((i,j), 10) | i <- [0..succ w], j <- [0,succ h]] ++
          [((i,j), 10) | i <- [0,succ w], j <- [1..h]]
    lps = [aij  | j <- [1..h], i <- [1..w], let aij = arr ! (i,j), arr ! (i-1,j) > aij, arr ! (i+1,j) > aij, arr ! (i,j-1) > aij, arr ! (i,j+1) > aij]

test1 = compute1 <$> readFile "sample.txt"

run1 = compute1 <$> readFile "input.txt"

{-
part 2
底からの、9未満の要素の連結成分を数えて、大きい方から3つの積を求めろと。まんどくせ。
何先生の自慢の「世界一速いラベル付けアルゴリズム」を調べてきた。
ようはUnion-Findの変形で、スキャン済みの近傍にラベルの付いたマスがなければ新たな番号でラベルを付ける、
もし複数の候補がある場合はその全てをunionする、で走査をすると、unionとしての連結成分が繋がるという仕掛けだった。

改めて定式化する。ここで使えるように。
h*wのピクセルに0～h*w-1の番号を割り振り、MVを-1(サイズ1の代表)で初期化する。
全てのピクセルについて、隣接する上と左がそれぞれ9未満、つまり白いピクセルならば、それぞれとunionする。
MVの最小から3つの値を取り出したら、それらがそれぞれのサイズ。
-}

compute2 :: String -> IO Int
compute2 xs = do
    let ls = lines xs
    let h = length ls
    let w = length (head ls)
    let xy2p = xy2pf w
    let arr = array ((1,1),(w, h)) $
              [((i,j), c /= '9') | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l]
    uf <- newUF (h * w)
    forM_ [1..h] (\j -> do
      forM_ [1..w] (\i -> do
        when (arr ! (i,j)) (do
          let p = xy2p i j
          when (i > 1 && arr ! (pred i, j)) (do
            let p1 = xy2p (pred i) j
            uniteUF uf p p1
            )
          when (j > 1 && arr ! (i, pred j)) (do
            let p2 = xy2p i (pred j)
            uniteUF uf p p2
            )
          )
        )
      )
    product . map negate . take 3 . sort <$> forM [0..h*w-1] (MUV.read uf)

xy2pf w x y = w * pred y + pred x

test2 = readFile "sample.txt" >>= compute2

run2 = readFile "input.txt" >>= compute2

{-
*Main> test1
15
*Main> run1
452
*Main> test2
1134
*Main> run2
1263735
-}

-- @gotoki_no_joe
type UnionFind = MUV.IOVector Int

newUF :: Int -> IO UnionFind
newUF n = MUV.replicate n (-1)

getRoot :: UnionFind -> Int -> IO Int
getRoot vec i = loop i []
  where
    loop :: Int -> [Int] -> IO Int
    loop i ks =
      do
        k <- MUV.read vec i
        if k < 0 then
          do
            forM_ ks (\k -> do MUV.write vec k i)
            return i
          else
            loop k (i:ks)

findUF :: UnionFind -> Int -> Int -> IO Bool
findUF vec i j =
 do
    a <- getRoot vec i
    b <- getRoot vec j
    return (a == b)

uniteUF :: UnionFind -> Int -> Int -> IO ()
uniteUF vec i j =
  do
    a <- getRoot vec i
    b <- getRoot vec j
    if a == b then return () else
      do
        r <- MUV.read vec a
        s <- MUV.read vec b
        if r < s then
          do
            MUV.write vec a b
            MUV.write vec b (r+s)
          else do
            MUV.write vec b a
            MUV.write vec a (r+s)
