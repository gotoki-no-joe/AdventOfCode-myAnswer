{-
サンプルもinput.txtも、実は、一方通行の壁で囲まれたマスにしか分岐がなくて、
単なる有向グラフの最長経路を探せと言っているだけなのでは？
-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

import Debug.Trace

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1

main1 = runner "input.txt" part1

checkBranch ls =
  [ (i,j)
  | ((i,j), '.') <- assocs fld, i > 1, i < h, all (('.' ==) . (fld !)) [(pred i,j), (succ i,j),(i, pred j),(i, succ j)]]
  where
    h = length ls
    w = length $ head ls
    fld = listArray ((1,1),(h,w)) $ concat ls :: UArray (Int,Int) Char

-- 大丈夫そう。

-- では、paint的な方法で距離を塗りつぶそう。
-- .から.に行くとき、普通に進む
-- >に行くとき、進める向きであることを確認し、距離を塗る。
-- 同じ方向のもう一歩先のマスも塗る。
-- そこから周囲を見渡して、入る向きのマスで距離が未確定のものがいたら、そこで止まる。
-- 自分が最後なら、その先にさらに進む。
-- これ、DFSでいけるな。というかその方がいいな。
-- 決して入ってこれない騙しの進入口がなければ、DFSで十分。

-- やりやすいように、周囲に矢印が複数ある分岐マスの記号を変えよう。

part1 ls = dist
  where
    h = length ls
    w = length $ head ls
    bnds = ((0,1),(succ h,w))
    wall = replicate w '#'
    fld0 = listArray bnds $ concat $ wall : ls ++ [wall] :: UArray (Int,Int) Char

    fld = fld0 // [((i,j),'+') | ((i,j),'.') <- assocs fld0, i > 1, i < h, 1 < length (filter (flip elem "^>v<" . (fld0 !)) [(pred i,j), (succ i,j),(i, pred j),(i, succ j)])]
    dist :: Int
    dist = runST $ do
      dist <- newArray bnds maxBound :: ST s (STUArray s (Int,Int) Int)
      iter <- fixST $ \iter -> return $ \(i,j) d -> do
--        traceShow (i,j,d,fld ! (i,j)) $ return ()
        writeArray dist (i,j) d
        let d1 = succ d
            ijNEWS@[pN,pS,pE,pW] = [(pred i, j),(succ i, j),(i, succ j),(i, pred j)]
            [cN,cS,cE,cW] = map (fld !) ijNEWS
            cH = fld ! (i,j)
        dNEWS <- forM ijNEWS (readArray dist)
        let [dN,dS,dE,dW] = dNEWS
        let dmax = maximum $ [dN | cN == 'v'] ++ [dS | cS == '^'] ++ [dE | cE == '<'] ++ [dW | cW == '>']
        case cH of
          '^' -> iter pN d1
          'v' -> iter pS d1
          '<' -> iter pW d1
          '>' -> iter pE d1
          '.' -> do
            when (cN /= '#' && dN == maxBound) $ iter pN d1
            when (cS /= '#' && dS == maxBound) $ iter pS d1
            when (cE /= '#' && dE == maxBound) $ iter pE d1
            when (cW /= '#' && dW == maxBound) $ iter pW d1
          '+' | dmax < maxBound -> do
            writeArray dist (i,j) $ succ dmax
            when (cN == '^') $ iter pN $ dmax + 2
            when (cS == 'v') $ iter pS $ dmax + 2
            when (cE == '>') $ iter pE $ dmax + 2
            when (cW == '<') $ iter pW $ dmax + 2
              | otherwise -> return ()
      iter (1,2) 0
      readArray dist (h, pred w)

-- と、安心させておいて、本番は後半だったか。
-- といっても、+マークは一度しか進入できず、それ以外は距離を測って、グラフにできる。

countCross ls = (length cs, cs)
  where
    h = length ls
    w = length $ head ls
    bnds = ((0,1),(succ h,w))
    wall = replicate w '#'
    fld0 = listArray bnds $ concat $ wall : ls ++ [wall] :: UArray (Int,Int) Char
    cs = [(i,j) | ((i,j),'.') <- assocs fld0, i > 1, i < h, 1 < length (filter (flip elem "^>v<" . (fld0 !)) [(pred i,j), (succ i,j),(i, pred j),(i, succ j)])]

{-
ghci> runner "sample.txt" countCross
(7,[(4,12),(6,4),(12,22),(14,6),(14,14),(20,14),(20,20)])
ghci> runner "input.txt" countCross
(34,[(6,32),(10,12),(12,76),(12,108),(14,54),(32,54),(34,124),(36,40),(38,20),(38,108),(42,86),(56,110),(60,60),(60,136),(62,8),(64,34),(64,88),(78,38),(86,82),(88,6),(90,62),(90,112),(90,124),(100,130),(102,16),(106,76),(108,54),(108,114),(114,32),(128,44),(132,138),(136,60),(136,110),(138,78)])

個数は結構あるけど、結局4方向しか接続はないので、34!通りという訳でもないので、
DFSで総当たりしても何とかなるのでは。
-}

{-
まず、出発地点と目標地点、それと全ての交差点について、
到達可能な経路がある点どうしの可能な距離を測る必要があるのだけど、これがまぁめんどくさい。
4方向全ての、可能な方向に出発してみて、どこかに到着したら終わり、とやる感じか。
-}
