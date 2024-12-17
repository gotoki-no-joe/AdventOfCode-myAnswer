{-
CPU問題

命令
0 adv div A 除算 被除数はAレジスタ 除数はコンボoperandの2^ つまり右シフト 結果はAレジスタに書き戻し
1 bxl xor B literal XOR  レジスタBとリテラルオペランド、結果はBレジスタに書き戻し
2 bst set B コンボoperandの下3ビットをBレジスタにロード
3 jnz Aレジスタが0のときNOP、さもなくばリテラルoperandで絶対ジャンプ
4 bxc xor B C レジスタBとCのXOR、結果はBへ書き戻し、オペランドは使わず読み飛ばし
5 out コンボoperandの下位3ビットを出力、複数の出力はコンマ区切り
6 bdv Aレジスタから読むadvの振る舞いのまま、保存先がBになったもの(!)
7 cdv 保存先がC

リテラルオペランド：そのまま3ビット整数

コンボオペランド
0-3 : そのまま
4-6 : レジスタA,B,C
7 : 出現しない

とりあえずインタプリタ作れ、と。
-}

import Data.List.Split
import Data.Char

import Data.Bits
import Debug.Trace

import Control.Monad
import Data.List

runner i f = do
  xs <- map read . wordsBy (not . isDigit) <$> readFile i
  print $ f xs

test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

data State = State {iP :: Int, regA :: Int, regB :: Int, regC :: Int}

part1 :: [Int] -> [Int]
part1 (rA:rB:rC:prog) = exec $ State {iP = 0, regA = rA, regB = rB, regC = rC}
  where
    n = length prog
    exec :: State -> [Int]
    exec st
      | iP st < 0 = error "negative iP"
      | iP st == pred n = error "cannot load operand"
      | pred n < iP st = [] -- halt
      | otherwise = (instr !! (prog !! iP st)) st

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x

    literal st = prog !! succ (iP st)

    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

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

{-
クワインだっけか。

エラーをerrorで起こす代わりに-1とか出させて、遅延評価を使って == になるものを線形探索する？
記号実行で場合を絞り込む？とりあえず動かそう。
-}

test2 = runner "samp2.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> Int
part2 (rA:rB:rC:prog) = head
  [ aval
  | aval <- [0 ..], aval /= rA, mod aval 10000 /= 0 || traceShow aval True
  , exec (State {iP = 0, regA = aval, regB = rB, regC = rC}) == prog]
  where
    n = length prog
    exec :: State -> [Int]
    exec st
      | iP st < 0 = [-1] -- error "negative iP"
      | iP st == pred n = [-2] -- error "cannot load operand"
      | pred n < iP st = [] -- halt
      | otherwise = (instr !! (prog !! iP st)) st

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x

    literal st = prog !! succ (iP st)

    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

    adv st = cc st $ exec $ st {iP = 2 + iP st, regA = shiftR (regA st) (combo st)}
    bxl st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (literal st)}
    bst st = cc st $ exec $ st {iP = 2 + iP st, regB = 7 .&. combo st}
    jnz st
      | regA st == 0 = exec $ st {iP = 2 + iP st}
      | otherwise    = exec $ st {iP = literal st}
    bxc st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (regC st)}
    out st = cc st $ 7 .&. combo st : exec (st {iP = 2 + iP st})
    bdv st = cc st $ exec $ st {iP = 2 + iP st, regB = shiftR (regA st) (combo st)}
    cdv st = cc st $ exec $ st {iP = 2 + iP st, regC = shiftR (regA st) (combo st)}

    cc st cont
      | literal st == 7 = [-3] -- invalid combo operand
      | otherwise = cont

{-
try = do
  xs <- map read . wordsBy (not . isDigit) <$> readFile "input.txt"
  part3 xs

part3 :: [Int] -> IO ()
part3 (rA:rB:rC:prog) = do
  forM_ [0 .. 7] (\a -> do
    let aval = 5 * 8 + a
    print (a, aval)
    print $ exec (State {iP = 0, regA = aval, regB = rB, regC = rC})
    )
  where
    n = length prog
    exec :: State -> [Int]
    exec st
      | iP st < 0 = [-1] -- error "negative iP"
      | iP st == pred n = [-2] -- error "cannot load operand"
      | pred n < iP st = [] -- halt
      | otherwise = (instr !! (prog !! iP st)) st

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x

    literal st = prog !! succ (iP st)

    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

    adv st = cc st $ exec $ st {iP = 2 + iP st, regA = shiftR (regA st) (combo st)}
    bxl st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (literal st)}
    bst st = cc st $ exec $ st {iP = 2 + iP st, regB = 7 .&. combo st}
    jnz st
      | regA st == 0 = exec $ st {iP = 2 + iP st}
      | otherwise    = exec $ st {iP = literal st}
    bxc st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (regC st)}
    out st = cc st $ 7 .&. combo st : exec (st {iP = 2 + iP st})
    bdv st = cc st $ exec $ st {iP = 2 + iP st, regB = shiftR (regA st) (combo st)}
    cdv st = cc st $ exec $ st {iP = 2 + iP st, regC = shiftR (regA st) (combo st)}

    cc st cont
      | literal st == 7 = [-3] -- invalid combo operand
      | otherwise = cont
-}

{-
プログラムの最後の出力を出せる、Aの3ビットの組み合わせを見つける。
それ*8+(0～7)で、プログラムの最後の2出力を出せる組み合わせを見つける。
を、プログラム全体になるまでのばしていく。
-}

part4 :: [Int] -> [Int]
part4 (rA:rB:rC:prog) = ans
  where
    n = length prog
    exec :: State -> [Int]
    exec st
      | iP st < 0 = [-1] -- error "negative iP"
      | iP st == pred n = [-2] -- error "cannot load operand"
      | pred n < iP st = [] -- halt
      | otherwise = (instr !! (prog !! iP st)) st

    combo st =
      case literal st of
        4 -> regA st
        5 -> regB st
        6 -> regC st
        7 -> error "invalid combo operand"
        x -> x

    literal st = prog !! succ (iP st)

    instr = [adv, bxl, bst, jnz, bxc, out, bdv, cdv]

    adv st = cc st $ exec $ st {iP = 2 + iP st, regA = shiftR (regA st) (combo st)}
    bxl st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (literal st)}
    bst st = cc st $ exec $ st {iP = 2 + iP st, regB = 7 .&. combo st}
    jnz st
      | regA st == 0 = exec $ st {iP = 2 + iP st}
      | otherwise    = exec $ st {iP = literal st}
    bxc st = exec $ st {iP = 2 + iP st, regB = xor (regB st) (regC st)}
    out st = cc st $ 7 .&. combo st : exec (st {iP = 2 + iP st})
    bdv st = cc st $ exec $ st {iP = 2 + iP st, regB = shiftR (regA st) (combo st)}
    cdv st = cc st $ exec $ st {iP = 2 + iP st, regC = shiftR (regA st) (combo st)}

    cc st cont
      | literal st == 7 = [-3] -- invalid combo operand
      | otherwise = cont

-- 後ろから順に調べていく
    ans = foldr step [0] $ init $ tails prog
    step progtail cands = traceShowId
      [ aval
      | c <- cands, d <- [0 .. 7], let aval = c * 8 + d
      , exec (State {iP = 0, regA = aval, regB = rB, regC = rC}) == progtail]

run4 i = do
  xs <- map read . wordsBy (not . isDigit) <$> readFile i
  print $ part4 xs

test4 = run4 "samp3.txt"
main4 = run4 "input.txt"