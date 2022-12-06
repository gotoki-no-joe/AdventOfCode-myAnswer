import Data.List.Split
import qualified Data.IntMap as IM
import Data.List
import qualified Data.IntSet as IS
import System.CPUTime
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as UV
import Control.Applicative
import Control.Monad

compute2 xs = --(xn*yn*zn, ans)
  do
    print total
    t0 <- getCPUTime
    vec <- MUV.replicate total False
    forM_ ls (\(b,[xl,xu,yl,yu,zl,zu]) -> do
      forM_ [p2iZ IM.! zl..pred (p2iZ IM.! succ zu)] (\zi -> do
        let i = zi * yn
        forM_ [p2iY IM.! yl..pred (p2iY IM.! succ yu)] (\yi -> do
          let j = (i + yi) * xn
          forM_ [p2iX IM.! xl..pred (p2iX IM.! succ xu)] (\xi -> do
            let k = j + xi
            MUV.write vec k b
            )
          )
        )
      )
    ans <- sum <$> forM [0..pred zn] (\zi -> do
      let i = zi * yn
      sum <$> forM [0..pred yn] (\yi -> do
        let j = (i + yi) * xn
        sum <$> forM [0..pred xn] (\xi -> do
          let k = j + xi
          b <- MUV.read vec k
          return $ if b then boxsize xi yi zi else 0
          )
        )
      )
    print ans
    t1 <- getCPUTime
    print $ t1 - t0
  where
    ls = map parseline $ lines xs
    (xls:xus:yls:yus:zls:zus:_) = transpose $ map snd ls
    (p2iX,i2wX) = partLines xls xus
    (p2iY,i2wY) = partLines yls yus
    (p2iZ,i2wZ) = partLines zls zus
    xn = UV.length i2wX
    yn = UV.length i2wY
    zn = UV.length i2wZ
    boxsize xi yi zi = (i2wX UV.! xi) * (i2wY UV.! yi) * (i2wZ UV.! zi)
    total = xn * yn * zn

partLines :: [Int] -> [Int] -> (IM.IntMap Int, UV.Vector Int)
partLines xls xus = (pos2ind,ind2wid)
  where
    xs = IS.elems $ IS.fromList (xls ++ map succ xus)
    pos2ind = IM.fromList $ zip xs [0..]
    ind2wid = UV.fromList $ zipWith (-) (tail xs) xs

parseline :: String -> (Bool,[Int])
parseline l = (onoff == "on", map read xyzlu)
  where
    onoff:xyz:_ = words l
    xyzlu = concatMap (splitOn ".." . drop 2) $ wordsBy (',' ==) xyz

test = readFile "sample3.txt" >>= compute2
run2 = readFile "input.txt" >>= compute2

main = run2

{-
GHCiで
*Main> test
1587808
2758514936282235
9140625000000

コンパイルして
>p2v2
570405888
p2v2: Stack space overflow: current size 33624 bytes.
p2v2: Relink with -rtsopts and use `+RTS -Ksize -RTS' to increase it.
あれっ？
>p2v2
570405888
p2v2: getMBlocks: VirtualAlloc MEM_COMMIT failed: (文字化け)
あれれれっ？
-}
