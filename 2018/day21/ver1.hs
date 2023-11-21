import Data.Array
import qualified Data.Map as M
import Data.Bits
import qualified Data.IntSet as IS

import Debug.Trace

{-
day19のver2を持ってきて、そのまま使う。
トレースを表示して、29のr0の値を観察する。
とやってみたら案外時間かかりそうだったので、PC=29でブレークするようにして観察。
(29,"addr 1 4 4",array (0,5) [(0,0),(1,0),(2,1),(3,2792537),(4,29),(5,1)])
ということで、ブレーク外して実行ずる。
-}

main1 :: IO ()
main1 = do
  ls <- tail . lines <$> readFile "input.txt"
  let ans = solve 4 2792537 ls
  print ans

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

solve :: Int -- PCなレジスタ番号
       -> Int -- レジスタ0の初期値
       -> [String] -- プログラム
       -> Int -- 停止時のレジスタの内容
solve ip r0 ls = loop 0 (listArray (0,5) $ r0 : repeat 0)
  where
    n = length ls
    prog = listArray (0, pred n) [opcode2f M.! o $ map read abcs | l <- ls, let o:abcs = words l]
    loop pc regs
      | n <= pc = regs ! 0
--      | trace (show (pc, ls !! pc, regs1)) False = error ""
--      | pc == 29 = trace (show (pc,ls !! pc, regs1)) regs ! 0
      | otherwise = loop (succ $ regs2 ! ip) regs2
      where
        regs1 = regs // [(ip, pc)]
        regs2 = (prog ! pc) regs1

{-
r0を-1とかにしておいて、PC=29のときにr3を表示して、それがループするとき、その手前の値が最大、で見つかるかしら？

計算を効率化していないと、ぜんぜん終わらないなこれ。しないとダメ？
-}

main2 :: IO ()
main2 = do
  ls <- tail . lines <$> readFile "input.txt"
  let ans = solve1 4 (-1) ls
  print ans

solve1 :: Int -- PCなレジスタ番号
       -> Int -- レジスタ0の初期値
       -> [String] -- プログラム
       -> Int -- 最大のr0の設定値
solve1 ip r0 ls = loop (listArray (0,5) $ r0 : repeat 0) IS.empty (-1)
  where
    n = length ls
    prog = listArray (0, pred n) [opcode2f M.! o $ map read abcs | l <- ls, let o:abcs = words l]
    loop regs r3s lastr3
      | n <= pc = -1 -- regs ! 0
      | pc == 29, IS.member r3 r3s = lastr3
      | pc == 26 = trace ("r2: " ++ show (regs2 ! 2)) $ loop regs2 (IS.insert r3 r3s) r3
      | pc == 29 = trace ("r3: " ++ show r3) $ loop regs2 (IS.insert r3 r3s) r3
      | otherwise = loop regs2 r3s lastr3
      where
        pc = regs ! ip;
        regs1 = (prog ! pc) regs
        regs2 = regs1 // [(ip, succ $ regs1 ! ip)]
        r3 = regs2 ! 3

step1 :: Int -> Int
step1 r3 = r31
  where
    r2 = r3 .|. 65536;
    r30 = 7637914;
    r31 = foldl inner r30 $ tt $ takeWhile (0 <) $ iterate (flip shiftR 8) r2
    inner r3 r2 = r34
      where
        r31 = r3 + r2 .&. 255
        r32 = r31 .&. 16777215
        r33 = r32 * 65889
        r34 = r33 .&. 16777215

tt x = trace (show x) x

r3s :: [Int]
r3s = iterate step 0
  where
    step r3 = foldl inner 7637914 $ takeWhile (0 <) $ iterate (flip div 256) $ r3 .|. 65536
    inner r3 r2 = (((r3 + (r2 .&. 255)) .&. 16777215) * 65899) .&. 16777215

ans0 :: Int
ans0 = last $ takeWhile (0 /=) $ tail r3s
-- これだと、前からずっとやるので当然大変。
-- stepの逆関数があれば、2792537になるstepの引数、で終わりだけど、知らないので、
-- むしろ0～0xFF_FFFF = 16777215 まで探した方が速いかな。

ans1 = head [r3 | r3 <- [0 .. 16777215], step r3 == 0]
  where
    step :: Int -> Int
    step r3 = foldl inner 7637914 $ takeWhile (0 <) $ iterate (flip div 256) $ r3 .|. 65536
    inner r3 r2 = (((r3 + (r2 .&. 255)) .&. 16777215) * 65899) .&. 16777215

{-
ghci> ans1
16539428
しばらく回したら答えが見つかった。...違うって。えー？
That's not the right answer; your answer is too high.
-}

ans2 = [r3 | r3 <- [0 .. 16777215], step r3 == r3]
  where
    step :: Int -> Int
    step r3 = foldl inner 7637914 $ takeWhile (0 <) $ iterate (flip div 256) $ r3 .|. 65536
    inner r3 r2 = (((r3 + (r2 .&. 255)) .&. 16777215) * 65899) .&. 16777215

part2 = loop (IS.singleton 0) r3s
  where
    loop is xs@(x1:x2:_)
      | IS.member x2 is = x1
      | otherwise = loop (IS.insert x2 is) (tail xs)

{-
ghci> part2
10721810
ghci> length $ takeWhile (10721810 /=) r3s
10677

なんか警戒してたのが肩透かしにさっくり答えが出たわ。とほほ。
-}