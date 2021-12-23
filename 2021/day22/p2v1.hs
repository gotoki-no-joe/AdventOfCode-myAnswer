import Data.List.Split
import Data.Array
import qualified Data.IntMap as IM
import Data.List
import qualified Data.IntSet as IS
import System.CPUTime

compute2 xs = (xn*yn*zn, ans)
  where
    ls = map parseline $ lines xs
    (xls:xus:yls:yus:zls:zus:_) = transpose $ map snd ls
    (p2iX,i2wX) = partLines xls xus
    (p2iY,i2wY) = partLines yls yus
    (p2iZ,i2wZ) = partLines zls zus
    (_,xn) = bounds i2wX
    (_,yn) = bounds i2wY
    (_,zn) = bounds i2wZ
    arr = accumArray (\_ x -> x) False ((1,1,1),(xn,yn,zn))
          [ ((xi,yi,zi), b)
          | (b,[xl,xu,yl,yu,zl,zu]) <- ls
          , xi <- [p2iX IM.! xl..pred (p2iX IM.! succ xu)]
          , yi <- [p2iY IM.! yl..pred (p2iY IM.! succ yu)]
          , zi <- [p2iZ IM.! zl..pred (p2iZ IM.! succ zu)] ]
    ans = sum $ map (boxsize . fst) $ filter snd $ assocs arr
    boxsize (xi,yi,zi) = (i2wX ! xi) * (i2wY ! yi) * (i2wZ ! zi)

partLines :: [Int] -> [Int] -> (IM.IntMap Int, Array Int Int)
partLines xls xus = (pos2ind,ind2wid)
  where
    xs = IS.elems $ IS.fromList (xls ++ map succ xus)
    pos2ind = IM.fromList $ zip xs [1..]
    ind2wid = listArray (1,pred $ length xs) $ zipWith (-) (tail xs) xs

parseline :: String -> (Bool,[Int])
parseline l = (onoff == "on", map read xyzlu)
  where
    onoff:xyz:_ = words l
    xyzlu = concatMap (splitOn ".." . drop 2) $ wordsBy (',' ==) xyz

test = readFile "sample3.txt" >>= print . compute2
run2 = readFile "input.txt" >>= print . compute2

main = do
  t0 <- getCPUTime
  run2
  t1 <- getCPUTime
  print $ t1 - t0

{-
*Main> test
(1587808,2758514936282235)
*Main> run2
(570405888,

う、実装は正しくできたっぽいのに、本番が完了しない。
420行使って、毎回違うところで切り続けたら、この方式で区切りの個数はいくつできるだろうか。

>ghc -O2 p2v1.hs
[1 of 1] Compiling Main             ( p2v1.hs, p2v1.o )
Linking p2v1.exe ...
>p2v1
(570405888,1233304599156793)
55171875000000

55秒。
-}
