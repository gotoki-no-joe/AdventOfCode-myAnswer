main = do
  fi <- readFile "input.txt"
  let li = head $ lines fi
  let (t,"") = parse li
  let ans1 = score 1 t
  print ans1
  putStrLn "part2"
  let ans2 = countGarbage t
  print ans2
  putStrLn "more compact solution"
  print (score2 li)
  print (countGarbage2 li)

data Stream = Group [Stream] | Garbage String

parse :: String -> (Stream, String)
parse ('{':cs) = (Group ts, cs1) where
  (ts,'}':cs1) = parseList cs
parse ('<':cs) = (Garbage gs, cs1) where
  (gs,cs1) = parseGarbage cs
-- parse x = error x

parseList cs@('}':_) = ([], cs)
parseList cs = (t1:ts, cs2) where
  (t1, cs1) = parse cs
  (ts, cs2) = parseList1 cs1

parseList1 cs@('}':_) = ([], cs)
parseList1 (',':cs) = (t1:ts, cs2) where
  (t1, cs1) = parse cs
  (ts, cs2) = parseList1 cs1
-- parseList1 x = error x

parseGarbage ('>':cs) = ("", cs)
parseGarbage ('!':c:cs) = (gs, cs1) where -- ('!':c:gs, cs1) where
  (gs, cs1) = parseGarbage cs
parseGarbage (c:cs) = (c:gs, cs1) where
  (gs, cs1) = parseGarbage cs

tests1 = ["{}","{{{}}}","{{},{}}","{{{},{},{{}}}}"
         ,"{<{},{},{{}}>}","{<a>,<a>,<a>,<a>}"
         ,"{{<a>},{<a>},{<a>},{<a>}}","{{<!>},{<!>},{<!>},{<a>}}"]

tests2 = ["{}", "{{{}}}", "{{},{}}", "{{{},{},{{}}}}"
        ,"{<a>,<a>,<a>,<a>}", "{{<ab>},{<ab>},{<ab>},{<ab>}}"
        ,"{{<!!>},{<!!>},{<!!>},{<!!>}}", "{{<a!>},{<a!>},{<a!>},{<ab>}}"]

countGroup (Group ss) = 1 + sum (map countGroup ss)
countGroup (Garbage _) = 0

test1 = map (countGroup . fst . parse) tests1

score :: Int -> Stream -> Int
score _ (Garbage _) = 0
score s (Group ss) = s + sum (map (score $! succ s) ss)

test2 = map (score 1. fst . parse) tests2

tests3 = ["<>","<random characters>","<<<<>","<{!>}>","<!!>","<!!!>>","<{o\"i!a,<{i<a>"]

countGarbage (Group ss) = sum (map countGarbage ss)
countGarbage (Garbage g) = length g

test3 = map (countGarbage . fst . parse) tests3

{-
*Main> main
14421
part2
6817

大した重さでもなかったが、結局木を作る必要性はなかった。
騙された。もっと簡単にやれる。
-}

countGroup2 :: String -> Int
countGroup2 = inGroup 0 where
  inGroup n ('{':cs) = (inGroup $! succ n) cs
  inGroup n (',':cs) = inGroup n cs
  inGroup n ('}':cs) = inGroup n cs
  inGroup n ('<':cs) = inGarbage n cs
  inGroup n "" = n
  inGarbage n ('>':cs) = inGroup n cs
  inGarbage n ('!':_:cs) = inGarbage n cs
  inGarbage n (_:cs) = inGarbage n cs
--  inGarbage n "" = n

test1' = map countGroup2 tests1

score2 :: String -> Int
score2 = inGroup 1 where
  inGroup s ('{':cs) = s + inGroup (succ s) cs
  inGroup s (',':cs) = inGroup s cs
  inGroup s ('}':cs) = inGroup (pred s) cs
  inGroup s ('<':cs) = inGarbage s cs
  inGroup s "" = 0
  inGarbage s ('>':cs) = inGroup s cs
  inGarbage s ('!':_:cs) = inGarbage s cs
  inGarbage s (_:cs) = inGarbage s cs
-- inGarbage s "" = 0

test2' = map score2 tests2

countGarbage2 :: String -> Int
countGarbage2 = inGroup 0 where
  inGroup c ('{':cs) = inGroup c cs
  inGroup c (',':cs) = inGroup c cs
  inGroup c ('}':cs) = inGroup c cs
  inGroup c ('<':cs) = inGarbage c cs
  inGroup c "" = c
  inGarbage c ('>':cs) = inGroup c cs
  inGarbage c ('!':_:cs) = inGarbage c cs
  inGarbage c (_:cs) = (inGarbage $! succ c) cs
  inGarbage c "" = c -- need for test

test3' = map countGarbage2 tests3

{-
*Main> main
14421
part2
6817
more compact solution
14421
6817
-}
