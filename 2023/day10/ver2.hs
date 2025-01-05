import Data.Array
import Data.Tuple
import Data.List.Split

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls
  print $ f fld

test1 = runner "samp1.txt" part12
test2 = runner "samp2.txt" part12
test3 = runner "samp3.txt" part12
test4 = runner "samp4.txt" part12
main1 = runner "input.txt" part12

type POS = (Int,Int)
type Agent = (POS,POS)

part12 fld = (ans, ans2)
  where
    toNorth p = elem (fld ! p) "|LJ"
    toSouth p = elem (fld ! p) "|7F"
    toEast  p = elem (fld ! p) "-LF"
    toWest  p = elem (fld ! p) "-J7"

    goNorth (i,j) = (pred i, j)
    goSouth (i,j) = (succ i, j)
    goEast  (i,j) = (i, succ j)
    goWest  (i,j) = (i, pred j)

-- Sの位置を探す
    posS = head [p | (p,'S') <- assocs fld]

-- Sの隣接マスで、Sの方に移動できるマスがエージェントの初期位置
    bnds = bounds fld
    [a1,a2] = [(p, posS) | let p = goNorth posS, inRange bnds p, toSouth p]
           ++ [(p, posS) | let p = goSouth posS, inRange bnds p, toNorth p]
           ++ [(p, posS) | let p = goEast  posS, inRange bnds p, toWest  p]
           ++ [(p, posS) | let p = goWest  posS, inRange bnds p, toEast  p]

-- 二人が同じ位置に到達するか、すれ違うまで歩数を数える。後者の場合1減らす
    ans = loop 1 a1 a2
    loop cnt a1 a2
      | fst a1 == fst a2 = cnt -- 同じ位置に到達
      | swap a1 == a2    = negate cnt -- すれ違った（本当はpred cntでよい）
      | otherwise = loop (succ cnt) (stepAgent a1) (stepAgent a2)

-- マスから行ける方角で、一つ前の位置と違う方へ、エージェントを一歩進める
    stepAgent (pos, prev) = (new, pos)
      where
        new = head $
              [p | toNorth pos, let p = goNorth pos, p /= prev]
          ++ [p | toSouth pos, let p = goSouth pos, p /= prev]
          ++ [p | toEast  pos, let p = goEast  pos, p /= prev]
          ++ [p | toWest  pos, let p = goWest  pos, p /= prev]

{-
ghci> test1
4
ghci> test2
8
ghci> main1
7063
-}

-- エージェントの軌跡をとり、壁の位置を全て集める
    walls = loop2 a1 a2
    loop2 a1 a2
      | fst a1 == fst a2 = [fst a1]
      | swap a1 == a2    = [] -- 不要だけど
      | otherwise = fst a1 : fst a2 : loop2 (stepAgent a1) (stepAgent a2)

-- 壁の位置はfldのまま、`S`の位置は妥当な壁に、その他の位置は `.` に置き換えたマップを作る
    fld2 = accumArray (flip const) '.' bnds $
           (posS, charS) : [(p, fld ! p) | p <- walls]

-- `S`の位置の壁は、a1とa2の位置に行けるもの
    charS
      | isN, isE = 'L'
      | isN, isS = '|'
      | isN, isW = 'J'
      | isE, isS = 'F'
      | isE, isW = '-'
      | isS, isW = '7'
      where
        a1a2 = map fst [a1, a2]
        isN = elem (goNorth posS) a1a2
        isS = elem (goSouth posS) a1a2
        isE = elem (goEast  posS) a1a2
        isW = elem (goWest  posS) a1a2

-- 行ごとに数えて合計する
    (_,(_,w)) = bnds
    ans2 = sum $ map countLine $ chunksOf w $ elems fld2

{- 嘘解答
-- 行について数える
  -- 壁の外
    countLine "" = 0
    countLine (x:xs)
      | elem x "|7J" = countIn xs
      | otherwise    = countLine xs
  -- 壁の中にいる
    countIn "" = 0
    countIn (x:xs)
      | elem x "|LF" = countLine xs
      | otherwise    = succ $ countIn xs
-}

-- 行について数える
  -- 壁の外
    countLine "" = 0
    countLine ('|':xs) = countIn xs
    countLine ('L':xs) = ontheWall True  xs
    countLine ('F':xs) = ontheWall False xs
    countLine ('.':xs) = countLine xs
  -- 壁の中にいる
    countIn "" = 0
    countIn ('|':xs) = countLine xs
    countIn ('L':xs) = ontheWall False xs
    countIn ('F':xs) = ontheWall True  xs
    countIn ('.':xs) = succ $ countIn xs
  -- 壁の上
    ontheWall goInif7 ('-':xs) = ontheWall goInif7 xs
    ontheWall goInif7 ('7':xs) = if goInif7 then countIn xs else countLine xs
    ontheWall goInif7 ('J':xs) = if goInif7 then countLine xs else countIn xs
