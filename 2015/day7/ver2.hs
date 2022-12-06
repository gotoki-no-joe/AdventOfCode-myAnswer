-- 2022-11-12
import System.IO.Unsafe
import Data.Word
import Data.Bits

import qualified Data.Map as M
import Data.Char

{-# NOINLINE ls #-}
ls :: [String]
ls = lines $ unsafePerformIO $ readFile "input.txt"
{-# NOINLINE testls #-}
testls = lines $ unsafePerformIO $ readFile "test.txt"

{-
まずはパーサをどれだけ真面目に書くかだが。
wordsでバラした後、ラストが出力先の信号名、その手前が "->"、それより前が全て式。
長さ1なら定数、（または信号名？）
長さ2ならnot (項)
長さ3なら(項) 二項演算 (項)
ANDの左とシフト演算の右だけが定数が現れるらしいが、一般化して作っておこう。

以前の解を見たら、演算をそのまま関数で表現していた。どうしよう。
面白いから真似しようか。
-}

-- 情報源のリスト、論理関数、結果の供給先
type Instr = ([String], [Word16] -> Word16, String)

fWire, fNot, fAnd, fOr, fSL, fSR :: [Word16] -> Word16
fWire [x] = id x
fNot  [x] = complement x
fAnd [x,y] = x .&. y
fOr  [x,y] = x .|. y
fSL  [x,y] = shiftL x (fromIntegral y)
fSR  [x,y] = shiftR x (fromIntegral y)

parse :: String -> Instr
parse xs =
  case length ws of
    3 -> ([ws !! 0], fWire, last ws)
    4 -> ([ws !! 1], fNot,  last ws)
    5 -> ([ws !! 0, ws !! 2], f $ ws !! 1, last ws)
    _ -> error "never"
  where
    ws = words xs
    f "AND" = fAnd
    f "OR"  = fOr
    f "LSHIFT" = fSL
    f "RSHIFT" = fSR
    f _ = error "never"

-- part1 Mapで遅延評価して、配線の結果を計算させる
part1Body :: [String] -> M.Map String Word16
part1Body ls = m
  where
    m = M.fromList [(tgt, f $ g ts) | l <- ls, let (ts,f,tgt) = parse l]
    g ts = [if all isDigit t then read t else m M.! t | t <- ts]

part1 :: [String] -> Word16
part1 = (M.! "a") . part1Body

{-
当時に作った、先行評価なプログラムと比べると圧倒的な速度で結果が出せた。わはは。
コードも簡潔だし。
-}

part2Body ls = m2
  where
    instrs = map parse ls
    m1 = M.fromList [(tgt, f $ g m1 ts) | (ts,f,tgt) <- instrs]
    g m ts = [if all isDigit t then read t else m M.! t | t <- ts]
    va = m1 M.! "a"
    m2 = M.fromList [(tgt, f $ g m2 ts) | (ts,f,tgt) <-  instrs ++ [([show va], fWire, "b")]]

part2 = (M.! "a") . part2Body

part2a ls = m M.! "a"
  where
    instrs = map parse ls
    m = M.fromList [(tgt, f $ map g ws) | (ws, f, tgt) <- instrs]
    g "b" = 3176 -- part1 ls
    g w
      | all isDigit w = read w
      | otherwise     = m M.! w
