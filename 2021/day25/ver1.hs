{-
これは命令型言語でサクッとやった方が早いかなぁ。immutable array必須でしょう。
さもないと、「現在の編集中状態」でなくて「今の状態」だけを見て次の状態を作る感じ。それでいけるか。

いや、「どこがどこを決めるか」が難しいな。*は>またはvとして、

">." ">*" ">." "v." ".."
".>" ">?" ".>" "v." "?."

>のいる位置は、進まれて空欄になるか進めずに固定されるか決まる。
>が手前にいる空欄は、進まれて埋まるか進むものがなくて空欄か決まる。

そんなに難しく考えなくても、「進めるなら進む、さもなくば現状維持」でいいのかしら。
あと、進んだ、を記録集計するのも面倒。
-}

import Data.List

line :: Char -> Int -> String -> (Bool,String)
line c len xs = (xs /= ys, ys)
  where
    ys = take len $ tail $ loop (last xs : xs ++ [head xs])
    loop (d : '.' : xs) | d == c = '.' : c : loop xs
    loop (x : xs) = x : loop xs
    loop [] = []

halfstep :: Char -> Int -> [String] -> (Bool, [String])
halfstep c len xss = (or bs, yss)
  where
    (bs, yss) = unzip $ map (line c len) xss

step :: Int -> Int -> [String] -> (Bool, [String])
step w h xss = (b || c, transpose zss)
  where
    (b,yss) = halfstep '>' w xss
    (c,zss) = halfstep 'v' h (transpose yss)

compute1 fn = do
  co <- readFile fn
  let xss = lines co
  let (w,h) = (length (head xss), length xss)
  print $ loop w h xss 1

loop :: Int -> Int -> [String] -> Int -> Int
loop w h xss cnt =
  case step w h xss of
    (False,_) -> cnt
    (True, yss) -> loop w h yss (succ cnt)

test1 = compute1 "sample.txt"

run1 = compute1 "input.txt"

{-
*Main> test1
58
*Main> run1
384
-}
