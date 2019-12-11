import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List.Split

main = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is
    print ans

{-
day9からIntCode Computer ただし IM. qualified
-}

-- pc, base, memory, input, output
interpret :: Int -> Integer -> IM.IntMap Integer -> [Integer] -> [Integer]
interpret pc base mem js = case opcode of
    1 -> arith (+)
    2 -> arith (*)
    3 -> interpret (pc + 2) base (memwrite ad1 (head js)) (tail js)
    4 -> av1 : interpret (pc + 2) base mem js
    5 -> jump (av1 /= 0)
    6 -> jump (av1 == 0)
    7 -> set1 (av1 <  av2)
    8 -> set1 (av1 == av2)
    9 -> interpret (pc + 2) (base + av1) mem js
    99 -> []
    x -> error (unwords ["Illegal Opcode", show pc, show x])
  where
    opint = mem IM.! pc
    opcode = opint `mod` 100
    am1 = opint `div`   100 `mod` 10
    am2 = opint `div`  1000 `mod` 10
    am3 = opint `div` 10000 `mod` 10
    ma a = IM.findWithDefault 0 (fromIntegral a) mem
    av1 = case am1 of
        0 -> ma (ma $ pc + 1)
        1 -> ma (pc + 1)
        2 -> ma (base + ma (pc + 1))
    av2 = case am2 of
        0 -> ma (ma $ pc + 2)
        1 -> ma (pc + 2)
        2 -> ma (base + ma (pc + 2))
    ad3 = case am3 of
        0 -> ma $ pc + 3
        2 -> base + ma (pc + 3)
    ad1 = case am1 of
        0 -> ma $ pc + 1
        2 -> base + ma (pc + 1)
    arith op = interpret (pc + 4) base (memwrite ad3 (op av1 av2)) js
    jump True  = interpret (fromIntegral av2) base mem js
    jump False = interpret (pc + 3) base mem js
    set1 b = interpret (pc + 4) base (memwrite ad3 (if b then 1 else 0)) js
    memwrite ad v = IM.insert (fromIntegral ad) v mem

{-
塗装する船体とロボットの状態を管理する
ロボットの位置 (x,y)
向き (dx,dy) は回転行列で旋回
塗装は座標から色の Map (Int,Int) Bool : Falseが黒（デフォルト）Trueが白

現在位置の色を読み取って、コンピュータに入力
塗り替える色と移動先を出力させる
状態を更新して繰り返し

コンピュータがどこで停止するのかが問題だ。移動後のようだ。
-}

type Hull = M.Map (Int,Int) Bool
type Env = (Hull, (Int,Int), (Int,Int))

compute :: [Integer] -> Int
compute is = length $ (\(m,_,_) -> M.keys m) $ last envs
  where
    ia = IM.fromList $ zip [0..] is
    outs = interpret 0 0 ia rs
    envs = scanl hullRobot initial (chunksOf 2 outs)
    rs = map getPixel envs

initial :: Env
initial = (M.empty, (0,0), (0,-1))

getPixel :: Env -> Integer
getPixel (m, pos, _) = if (M.findWithDefault False pos m) then 1 else 0

hullRobot :: Env -> [Integer] -> Env
hullRobot (m, pos, dir) [color, turn] = (m1, add pos dir1, dir1)
  where
    m1 = M.insert pos (color == 1) m
    dir1 = doTurn dir turn

add (x,y) (dx,dy) = (x+dx, y+dy)

-- left (0,-1) -> (1,0) -> (0,1) -> (-1,0)
doTurn (dx,dy) 1 = (-dy,dx)
-- right (0,-1) -> (-1,0) -> (0,1) -> (1,0)
doTurn (dx,dy) 0 = (dy,-dx)
-- なんか逆だった

{-
*Main> main
2594
-}

-- 初期値の変更と、マップの出力
main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute2 is
    putStrLn ans

compute2 :: [Integer] -> String
compute2 is =  unlines [ [if (M.findWithDefault False (x,y) mz) then '■' else '　' | x <- [x0..x1]] | y <- [y0..y1] ]
  where
    ia = IM.fromList $ zip [0..] is
    outs = interpret 0 0 ia rs
    envs = scanl hullRobot initial2 (chunksOf 2 outs)
    rs = map getPixel envs
    (mz,_,_) = last envs
    ps = M.keys mz
    xs = map fst ps
    x0 = minimum xs
    x1 = maximum xs
    ys = map snd ps
    y0 = minimum ys
    y1 = maximum ys

initial2 :: Env
initial2 = (M.singleton (0,0) True, (0,0), (0,-1))
-- initial2 = (M.empty, (0,0), (0,-1)) -- for part1 setting

{-
*Main> main2
　　■■　　■　　■　■■■■　■■■　　　　■■　■■■■　■　　■　■　　■　　　
　■　　■　■　■　　■　　　　■　　■　　　　■　■　　　　■　　■　■　■　　　　
　■　　■　■■　　　■■■　　■　　■　　　　■　■■■　　■■■■　■■　　　　　
　■■■■　■　■　　■　　　　■■■　　　　　■　■　　　　■　　■　■　■　　　　
　■　　■　■　■　　■　　　　■　■　　■　　■　■　　　　■　　■　■　■　　　　
　■　　■　■　　■　■■■■　■　　■　　■■　　■　　　　■　　■　■　　■　　　
-}
