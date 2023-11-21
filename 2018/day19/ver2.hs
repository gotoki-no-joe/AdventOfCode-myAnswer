import Data.Array
import qualified Data.Map as M
import Data.Bits

import Debug.Trace

main1 :: IO ()
main1 = do
  ls <- tail . lines <$> readFile "input.txt"
  let ans = solve2 3 0 ls
  print ans

sample = do
  ls <- tail . lines <$> readFile "sample.txt"
  let ans = solve2 0 0 ls
  print ans

solve1 :: Int -- PCなレジスタ番号
       -> [String] -- プログラム
       -> Int -- 停止時のレジスタの内容
solve1 ip ls = loop 0 (listArray (0,5) $ repeat 0)
  where
    n = length ls
    prog = listArray (0, pred n) [opcode2f M.! o $ map read abcs | l <- ls, let o:abcs = words l]
    loop pc regs
      | n <= pc = regs ! 0
--      | trace (show (pc, ls !! pc, regs1)) False = error ""
      | otherwise = loop (succ $ regs2 ! ip) regs2
      where
        regs1 = regs // [(ip, pc)]
        regs2 = (prog ! pc) regs1

type Regs = Array Int Int

oprr, opri, opir :: (Int -> Int -> Int) -> [Int] -> Regs -> Regs
oprr f [a,b,c] = \reg -> reg // [(c, f (reg ! a) (reg ! b))]
opri f [a,b,c] = \reg -> reg // [(c, f (reg ! a) b)]
opir f [a,b,c] = \reg -> reg // [(c, f a (reg ! b))]

cmp f x y = if f x y then 1 else 0

opcode2f :: M.Map String ([Int] -> Regs -> Regs)
opcode2f = M.fromList
  [("addr", oprr (+)), ("addi", opri (+))
  ,("mulr", oprr (*)), ("muli", opri (*))
  ,("banr", oprr (.&.)), ("bani", opri (.&.))
  ,("borr", oprr (.|.)), ("bori", opri (.|.))
  ,("setr", oprr const), ("seti", opir const)
  ,("gtir", opir (cmp (>))), ("gtri", opri (cmp (>))), ("gtrr", oprr (cmp (>)))
  ,("eqir", opir (cmp (==))), ("eqri", opri (cmp (==))), ("eqrr", oprr (cmp (==)))
  ]

{-
ひとつめの☆をとった版は失われているので、作り直す。
-}

main = do
  ls <- tail . lines <$> readFile "input.txt"
  let ans = solve2 3 1 ls
  print ans

solve2 :: Int -- PCなレジスタ番号
       -> Int -- レジスタ0の初期値
       -> [String] -- プログラム
       -> Int -- 停止時のレジスタの内容
solve2 ip r0 ls = loop 0 (listArray (0,5) $ r0 : repeat 0)
  where
    n = length ls
    prog = listArray (0, pred n) [opcode2f M.! o $ map read abcs | l <- ls, let o:abcs = words l]
    loop pc regs
      | n <= pc = regs ! 0
--      | trace (show (pc, ls !! pc, regs1)) False = error ""
      | otherwise = loop (succ $ regs2 ! ip) regs2
      where
        regs1 = regs // [(ip, pc)]
        regs2 = (prog ! pc) regs1
