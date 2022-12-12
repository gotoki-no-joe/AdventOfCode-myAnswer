{-
Sから始めて、幅優先探索で進めていけば埋められる。
ただひたすら面倒な感じ。

-}

import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Ix

test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

phase1 fn = do
  ls <- lines <$> readFile fn
  let w = length (head ls)
  let h = length ls
  let a2z = listArray ((1,1),(h,w)) $ map se2az $ concat ls :: UArray (Int,Int) Char
  let [spos] = locate 'S' ls
  let [epos] = locate 'E' ls
  da <- newArray ((1,1),(h,w)) (-1) :: IO (IOUArray (Int,Int) Int)
  loopWrap a2z da epos spos
  readArray da epos >>= print

se2az 'S' = 'a'
se2az 'E' = 'z'
se2az  c  =  c

locate v xss = [(i,j) | (i,xs) <- zip [1..] xss, (j,x) <- zip [1..] xs, v == x]

deltas = [(-1,0),(1,0),(0,-1),(0,1)]
add (x,y) (z,w) = (x+z, y+w)

loopWrap a2z da epos spos = loop 0 [spos] []
  where
    bnds = bounds a2z
    loop _ [] [] = return () -- FAIL?
    loop cnt [] qs = loop (succ cnt) qs []
    loop cnt (p:ps) qs =
      do
        b <- readArray da p
        if b >= 0 then loop cnt ps qs else do
          writeArray da p cnt
          let h1 = succ $ a2z ! p
--          let qs1 = [q | d <- deltas, let q = add p d, inRange bnds q, h1 >= a2z ! q]
--          loop cnt ps (qs1 ++ qs)
          qs1 <- foldM (\qs d -> do
            let q = add p d
            if not (inRange bnds q) || h1 < a2z ! q then return qs else do
              d <- readArray da q
              return $ if d < 0 then q:qs else qs
            ) qs deltas
          loop cnt ps qs1

-- 訪問済みのqを排除しないので少し無駄がある。
-- foldM (\d -> q = add p d, inRange bnds q, h1 >= a2z ! q, d <- readArray da q, return $ if d < 0 then q:qs else qs) qs deltas
-- とすればいいのかな？いかった。

{-
Part 2
やはりEから出発すればよかったかも。

「全ての'a'から出発してEまでの距離の最小値」はひどいので、
Eから出発して、pred h 以上の高さを持つ隣にだけ進む、を、マスが尽きるまで行って、
全てのaの位置の距離の最小値を求めよう。
初めからこれにすればpart1も終わってたが。
-}

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"

phase2 fn = do
  ls <- lines <$> readFile fn
  let w = length (head ls)
  let h = length ls
  let a2z = listArray ((1,1),(h,w)) $ map se2az $ concat ls :: UArray (Int,Int) Char
  let [epos] = locate 'E' ls
  da <- newArray ((1,1),(h,w)) (-1) :: IO (IOUArray (Int,Int) Int)
  loop2Wrap a2z da epos
  forM (locate 'S' ls ++ locate 'a' ls) (readArray da) >>= print . minimum . filter (0 <=)

loop2Wrap a2z da epos = loop 0 [epos] []
  where
    bnds = bounds a2z
    loop _ [] [] = return () -- Ende
    loop cnt [] qs = loop (succ cnt) qs []
    loop cnt (p:ps) qs =
      do
        b <- readArray da p
        if b >= 0 then loop cnt ps qs else do
          writeArray da p cnt
          let h1 = pred $ a2z ! p
          qs1 <- foldM (\qs d -> do
            let q = add p d
            if not (inRange bnds q) || h1 > a2z ! q then return qs else do
              d <- readArray da q
              return $ if d < 0 then q:qs else qs
            ) qs deltas
          loop cnt ps qs1
