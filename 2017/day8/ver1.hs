{-# Language Strict #-}

-- import Data.List.Split
import Data.Map

main = do
  fi <- readFile "input.txt"
  let ls = Prelude.map parse $ lines fi
  let regs = exec ls
  let ans1 = maximum $ elems regs
  print regs
  print ans1
  let regss = trace ls
  let ans2 = maximum $ concatMap elems regss
  print ans2

type Reg = String
type Cmd = (Reg,Int,Reg,Int->Bool) -- レジスタ間比較はなさそうなので。

parse :: String -> Cmd
parse cs = (r1, incdec * read ofs, r2, cmp) where
  [r1,pm,ofs,"if",r2,op,rhs] = words cs
  incdec = if pm == "inc" then 1 else -1
  r = read rhs
  cmp = case op of
    "==" -> (== r)
    "!=" -> (/= r)
    "<"  -> (<  r)
    "<=" -> (<= r)
    ">"  -> (>  r)
    ">=" -> (>= r)

exec = Prelude.foldl step empty

step regs (r1,ofs,r2,cmp) = if cmp v2 then insert r1 (v1+ofs) regs else regs where
  v1 = if member r1 regs then regs ! r1 else 0
  v2 = if member r2 regs then regs ! r2 else 0

trace = scanl step empty

{-

*Main> main
fromList [("aam",-3563),("bz",-5100),("d",-6837),("eai",5064),("ey",40),("fu",5385),("gx",-3129),("hk",5562),("hnh",-427),("hr",1829),("hri",412),("kp",-283),("lk",-791),("n",-5297),("oiy",5966),("pce",-6806),("phf",-101),("px",-547),("q",-5443),("rjt",4175),("sy",-3565),("vyo",-1488),("wez",5375),("x",-3335),("yl",-1261),("zbh",-517)]
5966
6347
-}
