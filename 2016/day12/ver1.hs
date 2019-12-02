import Data.Array

main = do
    cont <- readFile "input.txt"
    let p = parse cont
    let ans = exec p
    print ans

data Instr
    = CpyI Int Char
    | CpyR Char Char
    | Inc Char
    | Dec Char
    | JnzR Char Int
    | JnzI Int Int
    deriving Show

parse :: String -> [Instr]
parse cont = map (parse1.words) (lines cont)

parse1 :: [String] -> Instr
parse1 ["cpy", x, y] | isReg x = CpyR (head x) (head y)
                     | True    = CpyI (read x) (head y)
parse1 ["inc", x] = Inc (head x)
parse1 ["dec", x] = Dec (head x)
parse1 ["jnz", x, y] | isReg x = JnzR (head x) (read y)
                     | True    = JnzI (read x) (read y)

isReg :: String -> Bool
isReg s = elem s ["a", "b", "c", "d"]

exec :: [Instr] -> Int
exec p = run 0 (listArray ('a','d') [0,0,0,0])
  where
    ub = length p
    run ip reg | ip >= ub = reg ! 'a'
               | True    = case p !! ip of
        CpyI n r -> run (succ ip) (reg // [(r,n)])
        CpyR x y -> run (succ ip) (reg // [(y, reg ! x)])
        Inc x    -> run (succ ip) (reg // [(x, succ $ reg ! x)])
        Dec x    -> run (succ ip) (reg // [(x, pred $ reg ! x)])
        JnzR x y -> if reg ! x /= 0 then run (ip + y) reg else run (succ ip) reg
        JnzI x y -> if x /= 0 then run (ip + y) reg else run (succ ip) reg

{-
*Main> main
317993
-}

main2 = do
    cont <- readFile "input.txt"
    let p = parse cont
    let ans = exec2 p
    print ans

exec2 :: [Instr] -> Int
exec2 p = run 0 (listArray ('a','d') [0,0,1,0])
  where
    ub = length p
    pa = listArray (0, length p - 1) p
    run ip reg | ip >= ub = reg ! 'a'
               | True    = case pa ! ip of
        CpyI n r -> run (succ ip) (reg // [(r,n)])
        CpyR x y -> run (succ ip) (reg // [(y, reg ! x)])
        Inc x    -> run (succ ip) (reg // [(x, succ $ reg ! x)])
        Dec x    -> run (succ ip) (reg // [(x, pred $ reg ! x)])
        JnzR x y -> if reg ! x /= 0 then run (ip + y) reg else run (succ ip) reg
        JnzI x y -> if x /= 0 then run (ip + y) reg else run (succ ip) reg

{-
クソ重いので配列に切り替えてゴリ押しした
*Main> main2
9227647
-}
