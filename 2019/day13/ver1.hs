import qualified Data.IntMap as IM
import Data.List.Split
import Debug.Trace

-- day9からIntCode Computer ただし IM. qualified

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

codeBlock = 2
codePaddle = 3
codeBall = 4

compute :: [Integer] -> Integer -> Int
compute is x = length $ filter (\[_,_,c] -> c == x) outs
  where
    ia = IM.fromList $ zip [0..] is
    outs = chunksOf 3 $ interpret 0 0 ia []

main1 x = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is x
    print ans

{-
*Main> main1 codeBlock
207
*Main> main1 codePaddle
1
*Main> main1 codeBall
1
パドルは幅があったりしないようだ。
-}

-- part2

{-
GUIで可視化して観察できると面白いのだろうけど、Haskellだとつらい。
他の言語でやり直してみようか。
-}

-- パドルとボールが1つずつだから、追いつくように操作すればよい

main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute2 is
    print ans

compute2 :: [Integer] -> Integer
compute2 is = last $ last $ filter (\[x,y,_] -> x == -1 && y == 0) outs
  where
    ia = IM.insert 0 2 $ IM.fromList $ zip [0..] is
    outs = chunksOf 3 $ interpret 0 0 ia control
    control = player outs

-- ballのX座標とpaddleのX座標を合わせるように出力し続ける
player :: [[Integer]] -> [Integer]
player ins = loop0 ins where
  -- 情報なし
  loop0 ([x,_,3]:ins) = trace (unwords ["Pad", show x]) $ loop1 x ins
  loop0 ([x,_,4]:ins) = trace (unwords ["Bal", show x]) $ loop2 x ins
  loop0 (_:ins) = loop0 ins
  loop0 [] = []
  -- パドル既知
--  loop1 px ([x,_,4]:ins) = trace (unwords [">Bal", show x]) $ signum (x - px) : loop0 ins
  loop1 px ([x,_,4]:ins) = trace (unwords [">Bal", show x]) $ case signum (x - px) of
    0 -> 0 : loop1 px ins
    i -> i : loop0 ins
--  loop1 _  ([_,_,3]:ins) = error "double paddle"
  loop1 px (_:ins) = loop1 px ins
  loop1 _ [] = []
  -- ボール既知
--  loop2 bx ([x,_,3]:ins) = trace (unwords [">Pad", show x]) $ signum (bx - x) : loop0 ins
  loop2 bx ([x,_,3]:ins) = trace (unwords [">Pad", show x]) $ case signum (bx - x) of
    0 -> 0 : loop1 x ins
    i -> i : loop0 ins
--  loop2 _  ([_,_,4]:ins) = error "double ball"
  loop2 bx (_:ins) = loop2 bx ins
  loop2 _ [] = []

-- 入力が0でパドルが移動しなかったときは更新を行わないので、表示は交互にならないという落とし穴があった。

{-
*Main> main2
Bal 19
>Pad 21
:
:
Pad 22
>Bal 23
10247
-}
