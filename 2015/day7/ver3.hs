-- 2025-1-11 version

import Data.Word
import Data.Bits
import Data.Char

import qualified Data.Map as M

import Debug.Trace

type Instr =
  ( String   -- 出力先の信号名
  , ( String   -- 論理ゲートの名前 "-"（直結）,AND,OR,LSHIFT,RSHIFT,NOT
    , [String] -- 入力元の信号名または数字列、0～2要素
    )
  )

runner i f = readFile i >>= print . f . map parse . lines

parse :: String -> Instr
parse l = (last ws, lhs)
  where
    ws = words l
    ws0:ws1:ws2:_ = ws
    lhs = case length ws of
      3 -> ("-", [ws0])       -- 123 -> x, hoge -> a
      4 -> (ws0, [ws1])       -- NOT x -> h
      5 -> (ws1, [ws0, ws2])  -- x AND y -> d, x OR y -> e, x LSHIFT 2 -> f, y RSHIFT 2 -> g

buildMap :: [Instr] -> M.Map String Word16
buildMap is = m
  where
    m = M.fromList [(sig, eval lhs) | (sig, lhs) <- is]
    eval (op, args) = op2f (head op) $ map readSig args
    op2f '-' [x]    = x
    op2f 'A' [x, y] = x .&. y
    op2f 'O' [x, y] = x .|. y
    op2f 'L' [x, y] = shiftL x (fromIntegral y)
    op2f 'R' [x, y] = shiftR x (fromIntegral y)
    op2f 'N' [x]    = complement x
    readSig arg
      | all isDigit arg = read arg
      | otherwise       = m M.! arg

test1 = runner "test.txt" buildMap

part1 is = buildMap is M.! "a"

main1 = runner "input.txt" part1

part2 is = buildMap m1 M.! "a"
  where
    m1 = [if sig == "b" then (sig, lit3176) else i | i@(sig, lhs) <- is]
    lit3176 = ("-",["3176"])

main2 = runner "input.txt" part2
