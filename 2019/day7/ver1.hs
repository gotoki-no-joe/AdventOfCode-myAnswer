import Data.Array
import Data.List (permutations)

main = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute is
    print ans

{- 全部バカ正直にエミュレーションすると無駄すぎる。
位相設定0～4に対して入力と出力をメモるべきなのかも。
しかし入力範囲が設定できない… -}
{- そういう方向性ではなかった -}

compute :: [Integer] -> Integer
compute is = maximum os
  where
    ia = listArray (0, length is - 1) is
    exec p i = let [x] = interpret 0 ia [p,i] in x
    os = [ oe
      | pa <- [0..4], let oa = exec pa 0
      , pb <- [0..4], pb /= pa, let ob = exec pb oa
      , pc <- [0..4], pc `notElem` [pa,pb], let oc = exec pc ob
      , pd <- [0..4], pd `notElem` [pa,pb,pc], let od = exec pd oc
      , pe <- [0..4], pe `notElem` [pa,pb,pc,pd], let oe = exec pe od
      ]
-- Data.List.permutationsとか使うべきだが。

test1 = compute [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
test2 = compute [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
test3 = compute [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

interpret :: Int -> Array Int Integer -> [Integer] -> [Integer]
interpret pc mem js = case opcode of
    1 -> arith (+)
    2 -> arith (*)
    3 -> interpret (pc + 2) (memwrite (ma (pc + 1)) (head js)) (tail js)
    4 -> av1 : interpret (pc + 2) mem js
    5 -> jump (av1 /= 0)
    6 -> jump (av1 == 0)
    7 -> set1 (av1 <  av2)
    8 -> set1 (av1 == av2)
    99 -> []
    x -> error (unwords ["Illegal Opcode", show pc, show x])
  where
    opint = mem ! pc
    opcode = opint `mod` 100
    am1 = opint `div`  100 `mod` 10
    am2 = opint `div` 1000 `mod` 10
    ma a = mem ! (fromIntegral a)
    av1 = (if am1 == 0 then ma else id) (ma (pc + 1))
    av2 = (if am2 == 0 then ma else id) (ma (pc + 2))
    arith op = interpret (pc + 4) (memwrite (ma (pc + 3)) (op av1 av2)) js
    jump True  = interpret (fromIntegral av2) mem js
    jump False = interpret (pc + 3) mem js
    set1 b = interpret (pc + 4) (memwrite (ma (pc + 3)) (if b then 1 else 0)) js
    memwrite a v = mem // [(fromIntegral a, v)]

{-
*Main> main
46248
-}

main2 = do
    li <- readFile "input.txt"
    let is = read $ '[' : li ++ "]"
    let ans = compute2 is
    print ans

{- 遅延評価によるカッコイイ並列計算のエミュレーション -}
compute2 :: [Integer] -> Integer
compute2 is = maximum os
  where
    ia = listArray (0, length is - 1) is
    os = [exec ps | ps <- permutations [5,6,7,8,9]]
    exec [pa,pb,pc,pd,pe] =
      let
        osa = interpret 0 ia (pa:0:ose)
        osb = interpret 0 ia (pb:osa)
        osc = interpret 0 ia (pc:osb)
        osd = interpret 0 ia (pd:osc)
        ose = interpret 0 ia (pe:osd)
      in
        last ose

test4 = compute2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
test5 = compute2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

{-
*Main> main2
54163586
-}
