import Data.Char

sample1 = "1 + 2 * 3 + 4 * 5 + 6"
sample2 = "1 + (2 * 3) + (4 * (5 + 6))"
sample3 = "2 * 3 + (4 * 5)"
sample4 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
sample5 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
sample6 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

comp1 :: String -> Int
comp1 expr = let (x,[]) = eval1 $ filter (' ' /=) expr in x

{-
はい。

expr = factor (* factor)*
factor = term (+ term)*
term = digit | lpar expr rpar
-}

eval1 ts = expr ts

expr ts = exprLoop $ factor ts

exprLoop (x,'*':ts) = let (y,ts1) = factor ts in exprLoop (x*y, ts1)
exprLoop xts = xts

factor ts = factorLoop $ term ts

factorLoop (x,'+':ts) = let (y,ts1) = term ts in factorLoop (x+y, ts1)
factorLoop xts = xts

term ('(':ts) = let (x,')':ts1) = expr ts in (x,ts1)    
term (c:ts) = (digitToInt c, ts)

test1 = map comp1 [sample1,sample2,sample3,sample4,sample5,sample6]

ans1 = readFile "input.txt" >>= print . sum . map comp1 . lines

{-
*Main> test1
[231,51,46,1445,669060,23340]
*Main> ans1
388966573054664
-}
