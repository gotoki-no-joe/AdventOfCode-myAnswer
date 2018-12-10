{-# Language Strict #-}

import Data.Char
import Data.Bits
import Data.Word
import qualified Data.Map as M
import Data.Either
import Debug.Trace

type Wire = String
type Conn = ([Wire],[Word16]->Word16,Wire)

{- 手抜き読み込み
   これでも真面目にパーサ書くより楽だった罠。-}

{- と思ったら、ゲートの入力、しかもANDの左にだけ直接1と書いてあることがある。
   そういう名前のワイヤを登録する手法で逃げるか。 -}

main = do
  fi <- readFile "input.txt"
  let cs = map parse $ lines fi
  let ws = resolve cs (M.singleton "1" 1)
  let ans1 = ws M.! "a"
  print ans1
  let ws2 = resolve cs (M.fromList [("1",1),("b",ans1)])
  let ans2 = ws2 M.! "a"
  print ans2

parse :: String -> Conn
parse cs = parse1 (words cs)

parse1 [wi1,"->",wi] | all isDigit wi1 = ([], \_ -> read wi1,wi)
                     | otherwise = ([wi1], \[a] -> a, wi)
parse1 [wi1,"AND",wi2,"->",wi] = ([wi1,wi2], \[a,b] -> a .&. b, wi)
parse1 [wi1, "OR",wi2,"->",wi] = ([wi1,wi2], \[a,b] -> a .|. b, wi)
parse1 ["NOT", wi1, "->", wi]  = ([wi1], \[a] -> complement a, wi)
parse1 [wi1, "LSHIFT", ds, "->", wi] = ([wi1], \[a] -> shiftL a (read ds), wi)
parse1 [wi1, "RSHIFT", ds, "->", wi] = ([wi1], \[a] -> shiftR a (read ds), wi)
-- parse1 ws = trace (unwords ws) ([],const 0,"foo")

{-
ワイヤの値の評価
どうせ入力は1箇所しかないので、一度確定したら変化することはない。
未確定の接続リストと、確定済みのワイヤのマップをループ変数にして、
新たに確定したワイヤと値のリストで後者を更新し、
確定しなかった接続のリストを再度回す、でなくなるまで繰り返す。
-}

resolve :: [Conn] -> M.Map Wire Word16 -> M.Map Wire Word16
resolve [] ws = ws
resolve cs ws = next where
  (cs1, ws1) = partitionEithers $ map resolve1 cs
  next = if length ws1 == 0 then trace ("stop " ++ show (length cs)) ws else resolve cs1 (foldr (uncurry (M.insertWith (flip const))) ws ws1)
  resolve1 c@(vs,f,w)
    | all (flip M.member ws) vs = Right (w, f $ map (flip (M.findWithDefault 0) ws) vs)
--      let x = f $ map (flip (M.findWithDefault 0) ws) vs
--      in trace (w ++ " : " ++ show x) (Right (w,x))
    | otherwise = Left c

sample =
 [ "123 -> x"
 , "456 -> y"
 , "x AND y -> d"
 , "x OR y -> e"
 , "x LSHIFT 2 -> f"
 , "y RSHIFT 2 -> g"
 , "NOT x -> h"
 , "NOT y -> i"
 ]

test = resolve (map parse sample) (M.singleton "1" 1)

{-
*Main> main
3176
14710

しかし回路とはそういうものではないからなぁ。
-}
