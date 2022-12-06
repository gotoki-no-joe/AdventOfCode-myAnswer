-- 2022-11-25
import Data.Array
import Data.Ix

data Inst = Ialu (Int->Int) Reg | Ijmp (Int->Bool) Reg Int
type Reg = Int

parse :: String -> Inst
parse xs =
  case words xs of
    ["hlf", reg] -> Ialu (flip div 2) (regp reg)
    ["tpl", reg] -> Ialu (3 *)        (regp reg)
    ["inc", reg] -> Ialu succ         (regp reg)
    ["jmp", ofs] -> Ijmp (const True) undefined (readofs ofs)
    ["jie", reg, ofs] -> Ijmp even   (regp reg) (readofs ofs)
    ["jio", reg, ofs] -> Ijmp (1 ==) (regp reg) (readofs ofs)
    _ -> error "parse"
  where
    regp ('a':_) = 0
    regp ('b':_) = 1
    regp _ = error "regp"
    readofs ('+':cs) = read cs
    readofs cs = read cs

exec :: Array Int Inst -> Int -> Array Reg Int -> Array Reg Int
exec prog pc regF
  | not $ inRange (bounds prog) pc = regF
  | otherwise =
    case prog ! pc of
      Ialu f r -> exec prog (succ pc) (regF // [(r, f $ regF ! r)])
      Ijmp p r ofs | p (regF ! r) -> exec prog (pc + ofs) regF
                   | otherwise    -> exec prog (succ pc) regF

main = do
  co <- readFile "input.txt"
  let is = map parse $ lines co
  let prog = listArray (1, length is) is
  putStrLn "part 1"
  print $ exec prog 1 (listArray (0,1) [0,0])
  putStrLn "part 2"
  print $ exec prog 1 (listArray (0,1) [1,0])
