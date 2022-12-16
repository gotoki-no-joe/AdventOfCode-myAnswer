{-
データのX,Yの最大値、最小値を抽出したい。
Xの最小-1,最大+1の範囲でシミュレーションすればよさそう。
Yは一応0から、最大+1までおいといて、そこに到達するまでの石の個数を数える。
-}

import Text.Parsec
import Text.Parsec.String

import Control.Monad
import Data.Array.IO
import Data.Ix

pNumber :: Parser Int
pNumber = read <$> many1 digit

pair :: Parser (Int,Int)
pair = do
  x <- pNumber
  char ','
  y <- pNumber
  return (x,y)

line :: Parser [(Int,Int)]
line = sepBy1 pair (string " -> ")

pLines :: Parser [[(Int,Int)]]
pLines = sepEndBy line endOfLine

phase0 fn = do
  els <- parseFromFile pLines fn
  case els of
    Left err -> print err
    Right xyss -> phase0a xyss

phase0a :: [[(Int,Int)]] -> IO ()
phase0a xyss = do
  let xs = map fst $ concat xyss
  let (xmin, xmax) = (minimum xs, maximum xs)
  let ys = map snd $ concat xyss
  let (ymin, ymax) = (minimum ys, maximum ys)
  print (xmin, ymin, xmax, ymax)

phase1 fn = do
  els <- parseFromFile pLines fn
  case els of
    Left err -> print err
    Right xyss -> phase1a xyss

phase1a :: [[(Int,Int)]] -> IO ()
phase1a xyss = do
  let xs = map fst $ concat xyss
  let (xmin, xmax) = (minimum xs, maximum xs)
  let ys = map snd $ concat xyss
  let (ymin, ymax) = (minimum ys, maximum ys)
  arr <- newArray ((pred xmin, 0),(succ xmax, succ ymax)) False :: IO (IOUArray (Int,Int) Bool)
  forM_ xyss (\xys ->
    forM_ (zip xys $ tail xys) (\((x0,y0),(x1,y1)) -> do
      let (sx0,sx1) = minMax x0 x1
      let (sy0,sy1) = minMax y0 y1
      forM_ (range ((sx0,sy0),(sx1,sy1))) (\xy -> writeArray arr xy True)
      )
    )
  getBounds arr >>= dropRocks arr

minMax x y
  | x <= y    = (x,y)
  | otherwise = (y,x)

display :: IOUArray (Int,Int) Bool -> IO ()
display arr = do
  ((x0,y0),(x1,y1)) <- getBounds arr
  forM_ [y0..y1] (\y -> do
    forM_ [x0..x1] (\x -> do
      v <- readArray arr (x,y)
      putChar $ if v then '#' else '.'
      )
    putChar '\n'
    )

dropRocks :: IOUArray (Int,Int) Bool -> ((Int,Int),(Int,Int)) -> IO ()
dropRocks arr bnds = loop 0 >>= print
  where
    (_,(_,ymax)) = bnds
    loop cnt = do
      r <- stone arr 500 0
      if r then loop (succ cnt) else return cnt
    stone :: IOUArray (Int,Int) Bool -> Int -> Int -> IO Bool
    stone arr x y = do
      let y1 = succ y
      if y1 > ymax then return False else do
        r <- readArray arr (x,y1)
        if not r then stone arr x y1 else do
          r <- readArray arr (pred x, y1)
          if not r then stone arr (pred x) y1 else do
            r <- readArray arr (succ x, y1)
            if not r then stone arr (succ x) y1 else do
              writeArray arr (x,y) True
              return True

{-
arr の 0 は何もなし、1は壁、2は石、と区別しなくてもいいのか。
Falseが何もなし、Trueが何かあり

(500,0)からスタートして、ymaxを限度として、落ちるところまで落とす。
停止したらそこを塗ってTrueを返す。落ち抜けたらFalseを返す。
-}

test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

{-
part 2
ymax + 2 のところに無限に広がる壁を置けと。
横にはもう1広げたら十分なはず。
停止条件は、「下に抜ける」がなくなって、「500 0の位置のまま動けなかった」に変わる。
-}

phase2 fn = do
  els <- parseFromFile pLines fn
  case els of
    Left err -> print err
    Right xyss -> phase2a xyss

phase2a :: [[(Int,Int)]] -> IO ()
phase2a xyss = do
  let xs = map fst $ concat xyss
  let (xmin, xmax) = (minimum xs, maximum xs)
  let ys = map snd $ concat xyss
  let (ymin, ymax) = (minimum ys, maximum ys)
  let xmin1 = 500 - (ymax + 4)
  let xmax1 = 500 + (ymax + 4)
  arr <- newArray ((xmin1, 0),(xmax1, ymax + 2)) False :: IO (IOUArray (Int,Int) Bool)
  forM_ [xmin1 .. xmax1] (\x -> writeArray arr (x, ymax + 2) True) -- 無限の床
  forM_ xyss (\xys ->
    forM_ (zip xys $ tail xys) (\((x0,y0),(x1,y1)) -> do
      let (sx0,sx1) = minMax x0 x1
      let (sy0,sy1) = minMax y0 y1
      forM_ (range ((sx0,sy0),(sx1,sy1))) (\xy -> writeArray arr xy True)
      )
    )
  getBounds arr >>= dropRocks2 arr

dropRocks2 :: IOUArray (Int,Int) Bool -> ((Int,Int),(Int,Int)) -> IO ()
dropRocks2 arr bnds = loop 0 >>= print
  where
    (_,(_,ymax)) = bnds
    loop cnt = do
      r <- stone arr 500 0
      if r then loop (succ cnt) else return (succ cnt)
    stone :: IOUArray (Int,Int) Bool -> Int -> Int -> IO Bool
    stone arr x y = do
      let y1 = succ y
      r <- readArray arr (x,y1)
      if not r then stone arr x y1 else do
        r <- readArray arr (pred x, y1)
        if not r then stone arr (pred x) y1 else do
          r <- readArray arr (succ x, y1)
          if not r then stone arr (succ x) y1 else do
            writeArray arr (x,y) True
            return $ y > 0

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"
