{-
値の重複もあるので、最初の並びで何番目なのかを書いて置かないとわからなくなる。

(番号, 元の値) というタプルのリストで表したとして、
ある番号の値が今どこにあるのか、特定は難しいよなぁ。
命令型の双方向リンクリストならできるのか。
それぞれの要素は配列で管理してしまって、前と後ろの両方のリンクを書き換える。
それなら常に要素はそこにある。

同等の計算を、もっと関数型らしくするのは難しそうだ。

違うと言われてかなり悩んだ。
サンプルだと要素数7個、数が最大4で、自分を超えることはないから助かったが、
実データだと、一周5000要素を超える数が平気で指定されている。
このとき、先に自分を外してしまうと、その分位置がずれるんだ。
これは、n-1でmodしてやればいいのかな。そうしたら、forwardだけで実現できるかな。
-}

import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Data.List

-- 前向きリンクと後ろ向きリンクの対
type DLL = (MUV.IOVector Int, MUV.IOVector Int)

-- 0からn-1の双方向リンク循環リストを作る
newDLL :: Int -> IO DLL
newDLL n = do
  vF <- MUV.generate n succ
  MUV.write vF (pred n) 0
  vB <- MUV.generate n pred
  MUV.write vB 0 (pred n)
  return (vF, vB)

-- 双方向リンクリストから、指定された要素を切り離す
-- つまり、その前後の要素を互いに接続する
-- x <-> y <-> z ==> x <-> z, [y]
deleteDLL :: Int -> DLL -> IO ()
deleteDLL y (vF,vB) = do
  x <- MUV.read vB y
  z <- MUV.read vF y
  MUV.write vF x z
  MUV.write vB z x
  return ()

-- 双方向リンクリストに、切り離されている要素yを、要素xの次の位置に挿入する
-- x <-> z, [y] ==> x <-> y <-> z
insertDLL :: Int -> Int -> DLL -> IO ()
insertDLL x y (vF,vB) = do
  z <- MUV.exchange vF x y
  MUV.write vF y z
  MUV.write vB z y
  MUV.write vB y x
  return ()

-- 双方向リンクリストで x の次の要素の番号を返す
forward :: Int -> DLL -> IO Int
forward x (vF,_) = MUV.read vF x

phase1 dbg fn = do
  ds <- map read . lines <$> readFile fn
  let n = length ds
  dll <- newDLL n
  forM_ (zip [0..] ds) (\(i, d) -> do
    let d1 = mod d (pred n)
    if d1 == 0 then return () else do
      j <- foldM (\j _ -> forward j dll) i [1 .. d1]
      deleteDLL i dll
      insertDLL j i dll
    )
-- デバッグ用 位置0から始めて、n要素をリンクリストの順に出力する
  when dbg $  foldM_ (\i _ -> do
    putStr $ show $ ds !! i
    putChar ','
    forward i dll
    ) 0 [1..n] >> putChar '\n'
-- 0の位置から1000,2000,3000先の値を取り出す
  let Just zero = elemIndex 0 ds
  i1 <- foldM (\j _ -> forward j dll) zero [1..1000]
  i2 <- foldM (\j _ -> forward j dll) i1 [1..1000]
  i3 <- foldM (\j _ -> forward j dll) i2 [1..1000]
  let (v1,v2,v3) = (ds !! i1, ds !! i2, ds !! i3)
  print (v1,v2,v3,v1+v2+v3)

test1 = phase1 True "test.txt"
main1 = phase1 False "input.txt"

theKey = 811589153

phase2 dbg fn = do
  ds <- map ((theKey *) . read) . lines <$> readFile fn
  let n = length ds
  dll <- newDLL n
-- mixing
  forM_ [1..10] (\_ -> do
    forM_ (zip [0..] ds) (\(i, d) -> do
      let d1 = mod d (pred n)
      if d1 == 0 then return () else do
        j <- foldM (\j _ -> forward j dll) i [1 .. d1]
        deleteDLL i dll
        insertDLL j i dll
      )
-- デバッグ用 位置0から始めて、n要素をリンクリストの順に出力する
    when dbg $ foldM_ (\i _ -> do
      putStr $ show $ ds !! i
      putChar ','
      forward i dll
      ) 0 [1..n] >> putChar '\n'
    )
-- 0の位置から1000,2000,3000先の値を取り出す
  let Just zero = elemIndex 0 ds
  i1 <- foldM (\j _ -> forward j dll) zero [1..1000]
  i2 <- foldM (\j _ -> forward j dll) i1 [1..1000]
  i3 <- foldM (\j _ -> forward j dll) i2 [1..1000]
  let (v1,v2,v3) = (ds !! i1, ds !! i2, ds !! i3)
  print (v1,v2,v3,v1+v2+v3)

test2 = phase2 True "test.txt"
main2 = phase2 False "input.txt"

{-
インタプリタだとちょっと待たされる感じ。
コンパイルして実行したら一瞬。
-}

main = do
  test1
  main1
  test2
  main2
