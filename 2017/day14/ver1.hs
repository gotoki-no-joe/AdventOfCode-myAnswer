import Data.List.Split
import Data.Bits
import Data.Char
import Data.Array.IO
import Data.List
import Control.Monad

-- 結び目ハッシュ
-- トップレベル

dohash :: String -> [Int]
dohash str = hex16 where
  lens0 = map fromEnum str ++ [17, 31, 73, 47, 23]
  sparse = run2 lens0
  hex16 = map (foldr1 xor) $ chunksOf 16 sparse
--  dense = [ intToDigit c | x <- hex16, let (a,b) = divMod x 16, c <- [a,b] ]

-- 64phaseした後に1回だけ終了処理をする

run2 dat = bs ++ as where
  (str,cur,_) = (!! 64) $ iterate (phase dat) ([0..255], 0, 0)
  cur1 = cur `mod` 256
  (as,bs) = splitAt (256 - cur1) str

-- runの終了処理をなくす

phase dat st = foldl (step 256) st dat

type State =
  ([Int]  -- ひも : 先頭は現在位置
  ,Int    -- 現在位置
  ,Int    -- スキップサイズ
  )

step :: Int   -- ひもの長さ
     -> State -- 状態
     -> Int   -- 今回ひねる長さ
     -> State
step base (str, cur, skip) len = (ds ++ cs, cur1, skip1) where
  (as,bs) = splitAt len str
  (cs,ds) = splitAt skip (bs ++ reverse as)
  cur1 = cur + len + skip
  skip1 = (skip + 1) `mod` base

-- 問題の部分

key = "jzgqcdpd-"

theBitmap = map (\ i -> dohash (key ++ show i)) [0..127]

ans1 = sum $ map popCount $ concat theBitmap

{-
後半は普通に4近傍の連結成分を全て取り出せという話。
どうやってやるのかしら。
ミクロに近隣の情報だけから何とかなる気がしない。
ちまちまと隣接をpaint文的に拾っていくしかないのかなぁ？

命令的でhaskellらしくない計算だぬ。

再書き換え可能な配列があるなら、順に書いていけば何とかなりそうだ。

n = 1 // 割り当て番号
b = array[128][128] // 元ビットマップ
a = array[128][128] // 書き換えカラーマップ
for (p in b)
  b[p] == 0 なら a[p] = 0 // 壁
  b[p] == 1 なら
     (b[p^],b[p<]) の場合分け
        (0,0) なら a[p] = n++
        (1,0) なら a[p] = a[p^]
        (0,1) なら a[p] = a[p<]
        (1,1) なら (x,y) = minmax a[p^] a[p^], a[p] = x, a[]y = x
aの中の使われている正の数の種類を数えたら答え
-}

compute2 = do
  ar <- newArray ((0,0),(127,127)) 0
  loop ar 0 0 1
  ns <- getElems ar
  let ans2 = length $ group $ sort ns
  print ans2

type AR = IOArray (Int,Int) Int

loop :: AR -- 配列
     -> Int -- Y座標
     -> Int -- X座標
     -> Int -- n
     -> IO ()
loop ar 128 0 _ = return ()
loop ar y 128 n = loop ar (succ y) 0 n
loop ar y x n
  | not (getb y x) = writeArray ar (y,x) 0 >> loop ar y (succ x) n
  | otherwise =
      case (getb (pred y) x, getb y (pred x)) of
        (False,False) -> writeArray ar (y,x) n >> loop ar y (succ x) (succ n)
        (True ,False) -> rest (pred y) x
        (False,True ) -> rest y (pred x)
        (True ,True ) -> heavyChoice
  where
    rest y1 x1 = do
      t <- readArray ar (y1, x1)
      writeArray ar (y, x) t
      loop ar y (succ x) n
    heavyChoice = do
      t1 <- readArray ar (pred y, x)
      t2 <- readArray ar (y, pred x)
      writeArray ar (y,x) t1
      let (a,b) = (min t1 t2, max t1 t2)
      forM [0..pred y] (\ y1 -> forM [0..127] (\ x1 -> do
        t <- readArray ar (y1, x1)
        if t == b then writeArray ar (y1,x1) a else return ()
        ))
      forM [0..x] (\ x1 -> do
        t <- readArray ar (y, x1)
        if t == b then writeArray ar (y,x1) a else return ()
        )
      loop ar y (succ x) n

getb y x
  | y < 0 || x < 0 = False
  | True = testBit (theBitmap !! y !! (x `div` 8)) (7 - x `mod` 8)

{-
*Main> ans1
8074
*Main> compute2
1213
-}
