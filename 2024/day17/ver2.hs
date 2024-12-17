import Data.List.Split
import Data.Char
import Data.Bits

import Data.List

import Debug.Trace

runner i f = do
  xs <- map read . wordsBy (not . isDigit) <$> readFile i
  print $ f xs

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int]
part1 = runCPU

data State = State {iP :: Int, regA :: Int, regB :: Int, regC :: Int}

runCPU :: [Int]  -- レジスタ初期値＋プログラム、入力データ
       -> [Int]  -- out出力の系列
runCPU (rA0:rB0:rC0:prog) = exec state0
  where
    ub = pred $ length prog -- アドレス上限
    state0 = State {iP = 0, regA = rA0, regB = rB0, regC = rC0}

    exec :: State -> [Int]
    exec st
      | pc < 0    = error "negative iP"
      | pc == ub  = error "cannot load operand"
      | ub < pc   = [] -- halt
      | otherwise = (instr !! (prog !! pc)) st
      where
        pc = iP st

    instr :: [State -> [Int]]
    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

    literal st = prog !! succ (iP st)

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x

    adv st = exec $ st {iP = 2 + iP st, regA = shiftR (regA st) (combo st)}
    bxl st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (literal st)}
    bst st = exec $ st {iP = 2 + iP st, regB = 7 .&. combo st}
    jnz st
      | regA st == 0 = exec $ st {iP = 2 + iP st}
      | otherwise    = exec $ st {iP = literal st}
    bxc st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (regC st)}
    out st = 7 .&. combo st : exec (st {iP = 2 + iP st})
    bdv st = exec $ st {iP = 2 + iP st, regB = shiftR (regA st) (combo st)}
    cdv st = exec $ st {iP = 2 + iP st, regC = shiftR (regA st) (combo st)}

test2 = runner "samp2.txt" part2
main2 = runner "input.txt" part2

{-
part2 :: [Int] -> Int
part2 (_:xs) = head
  [ a
  | a <- [0 ..], mod a 10000 /= 0 || traceShow a True
  , runCPU (a : xs) == drop 2 xs]
-}
part2 :: [Int] -> [Int]
part2 (_:xs) = ans
  where
    prog = drop 2 xs
    ans = foldr step [0] $ init $ tails prog
    step progtail cands = traceShowId -- を差し込むと様子を見られる
      [ a
      | c <- cands, d <- [0 .. 7], let a = c * 8 + d
      , runCPU (a:xs) == progtail]
