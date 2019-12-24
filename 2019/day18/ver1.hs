{- IOUArray で書き換え的計算をやってみよう -}

--main = do
--  co <- readFile "input.txt"

import Data.Array.IO
import Control.Monad
import Data.Char

type MuMap = IOUArray (Int,Int) Char

sample1 = "#########\n#b.A.@.a#\n#########\n"

buildMap :: String -> IO MuMap
buildMap mapstr = do
  let ls = lines mapstr
  let w = pred $ length $ head ls
  let h = pred $ length ls
  maparr <- newListArray ((0,0),(h,w)) $ concat ls
  return maparr

findErase :: Char -> MuMap -> IO [(Int,Int)]
findErase c maparr = do
    assocs <- getAssocs maparr
    let poss = map fst $ filter ((c ==).snd) assocs
    forM poss (\p -> writeArray maparr p '.')
    return poss

showMap :: MuMap -> IO ()
showMap maparr = do
    (a,b) <- getBounds maparr
    forM_ [fst a..fst b] (\y -> do
        forM_ [snd a..snd b] (\x -> do
            c <- readArray maparr (y,x)
            putChar c
            )
        putChar '\n'
        )

-- 出発地点から到達できる鍵とそこまでの距離を全て挙げる
findPaths :: MuMap -> (Int,Int) -> IO ([(Char,Int)])
findPaths maparr0 p0 = do
    maparr1 <- mapArray id maparr0
    loop maparr1 0 [p0]
  where
{- loop ma d ps : 
      地図maの点p in psを、
      空白なら（距離dであるとして）塗り、その周囲を次に調べるように保存する
      鍵なら発見した成果として保存する
      それ以外なら何もしない
    保存した次に調べる場所で再帰する
    戻ってきた結果に自分の成果を付け足して帰る -}
    loop :: MuMap -> Int -> [(Int,Int)] -> IO [(Char,Int)]
    loop _  _ [] = return []
    loop ma d ps = do
        pairs <- forM ps (\p -> do
            c <- readArray ma p
            if isLower c then return ([(c, d)],[])
            else if c /= '.' then return ([],[])
            else do
                writeArray ma p '='
                return ([],adjacent p)
            )
        let (res0,p1s0) = unzip pairs
        let (res,p1s) = (concat res0, concat p1s0)
        res2 <- loop ma (succ d) p1s
        return (res ++ res2)

adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

recursion ma c ss d0 = do
    ma1 <- mapArray id ma
    [p0] <- findErase c ma1
    findErase (toUpper c) ma1
    cds <- findPaths ma1 p0
    if null cds then return [(d0,ss)]
    else do
        ress <- forM cds (\(c,d) -> do
            recursion ma1 c (c:ss) (d0+d)
            )
        return $ concat ress

compute1 ms = do
    ma <- buildMap ms
    ress <- recursion ma '@' "" 0
    return $ minimum ress

sample2 = "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################\n"
sample3 = "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################\n"
sample4 = "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################\n"
sample5 = "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################\n"

main1 = do
    co <- readFile "input.txt"
    ma <- buildMap co
    ress <- recursion ma '@' "" 0
    print ress

{-
鍵が多くなってくると、場合の数が増えて大変なことになる。
このアプローチのままでも、これまでの最小記録を中間結果が超えたら打ち切り、ということはできるだろう。
そうでなければ、@から鍵、鍵から鍵の全ての経路について、距離と間にある扉のリストを求めて、
今持っている鍵だけで進める場所に限って全ての頂点を踏破する経路を求める、というやり方の方か軽くなるのかな？
-}