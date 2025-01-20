import Data.Array

data Instr                  -- 命令
    = CpyI Int  Char        -- 定数ロード
    | CpyR Char Char        -- レジスタ間転送
    | Inc Char
    | Dec Char
    | Jnz Char Int
    | J Int
    | NOP

parse l =
  case words l of
    ['c':_, x, y:_] | isReg x -> CpyR (head x) y
                  | otherwise -> CpyI (read x) y
    ['i':_, x:_]              -> Inc x
    ['d':_, x:_]              -> Dec x
    ['j':_, x, y]   | isReg x -> Jnz (head x) (read y)
                | read x == 0 -> NOP
                  | otherwise -> J (read y)

isReg (c:cs) = elem c "abcd" && null cs

runner i f = do
  is <- map parse . lines <$> readFile i
  let prog = listArray (1, length is) is
  print $ f prog

exec prog xs = loop (fst bnds) (listArray ('a','d') xs)
  where
    bnds = bounds prog
    loop ip regs
      | not $ inRange bnds ip = regs ! 'a'
      | otherwise =
          case prog ! ip of
            CpyI i c -> loop (succ ip) (regs // [(c, i)])
            CpyR x y -> loop (succ ip) (regs // [(y, regs ! x)])
            Inc x    -> loop (succ ip) (regs // [(x, succ $ regs ! x)])
            Dec x    -> loop (succ ip) (regs // [(x, pred $ regs ! x)])
            Jnz x y  -> loop (if regs ! x /= 0 then ip + y else succ ip) regs
            J   y    -> loop (ip + y) regs
            NOP      -> loop (succ ip) regs

part1 prog = exec prog [0,0,0,0]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

main2 = runner "input.txt" (\prog -> exec prog [0,0,1,0])

{-
ghci> test1
42
(0.00 secs, 109,152 bytes)
ghci> main1
317993
(1.90 secs, 713,360,256 bytes)
ghci> :r
[1 of 2] Compiling Main             ( ver5.hs, interpreted ) [Source file changed]
Ok, one module loaded.
ghci> main2
9227647
(58.78 secs, 20,697,357,904 bytes)

当時の計算機だともう少し辛い時間がかかった。
-}

--- transpile hand result
sample = f0 where
  f0 a b c d = f1 41 b c d
  f1 a b c d = f2 (succ a) b c d
  f2 a b c d = f3 (succ a) b c d
  f3 a b c d = f4 (pred a) b c d
  f4 a b c d = (if  a /= 0 then f6 else f5 ) a b c d
  f5 a b c d = f6 (pred a) b c d
  f6 a _ _ _ = a

part3 prog = unlines $ zipWith f [0 ..] prog ++ [trailer]
  where
    regf t rep = [if t == s then rep else [s] | s <- "abcd"]
    f i (CpyI v t) = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t (show v)
    f i (CpyR u t) = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t [u]
    f i (Inc t)    = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t ("(succ " ++ t : ")")
    f i (Dec t)    = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i)] ++ regf t ("(pred " ++ t : ")")
    f i (Jnz t y)  = unwords $ ["  f"++ show i, "a b c d = (if ", t : " /= 0 then f" ++ show (i + y), "else f" ++ show (succ i), ") a b c d"]
    f i (J y)      = unwords $ ["  f"++ show i, "a b c d = f"++ show (i + y), "a b c d"]
    f i  NOP       = unwords $ ["  f"++ show i, "a b c d = f"++ show (succ i), "a b c d"]
    trailer = "  f" ++ show (length prog) ++ " a _ _ _ = a"

{- printf使うべきだったかな でも後半がな。
-}

runner2 i f = readFile i >>= putStr . f . map parse . lines

-- runner2 "input.txt" part3 で生成したもの
input = f0 where
  f0 a b c d = f1 1 b c d
  f1 a b c d = f2 a 1 c d
  f2 a b c d = f3 a b c 26
  f3 a b c d = (if  c /= 0 then f5 else f4 ) a b c d
  f4 a b c d = f9 a b c d
  f5 a b c d = f6 a b 7 d
  f6 a b c d = f7 a b c (succ d)
  f7 a b c d = f8 a b (pred c) d
  f8 a b c d = (if  c /= 0 then f6 else f9 ) a b c d
  f9 a b c d = f10 a b a d
  f10 a b c d = f11 (succ a) b c d
  f11 a b c d = f12 a (pred b) c d
  f12 a b c d = (if  b /= 0 then f10 else f13 ) a b c d
  f13 a b c d = f14 a c c d
  f14 a b c d = f15 a b c (pred d)
  f15 a b c d = (if  d /= 0 then f9 else f16 ) a b c d
  f16 a b c d = f17 a b 13 d
  f17 a b c d = f18 a b c 14
  f18 a b c d = f19 (succ a) b c d
  f19 a b c d = f20 a b c (pred d)
  f20 a b c d = (if  d /= 0 then f18 else f21 ) a b c d
  f21 a b c d = f22 a b (pred c) d
  f22 a b c d = (if  c /= 0 then f17 else f23 ) a b c d
  f23 a _ _ _ = a

{-
ghci> input 0 0 0 0
317993
(0.46 secs, 163,195,568 bytes)
ghci> input 0 0 1 0
9227647
(16.28 secs, 4,739,533,776 bytes)
-}
