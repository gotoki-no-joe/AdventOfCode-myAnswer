-- 2022-12-10
import Data.List.Split

{-
命令列によって、時間が流れてXの値が変化していく。
次のクロックの開始時のXの値を生成するループを書こう。
-}

loop :: Int -> [String] -> [Int]
loop x (cs:css) =
  case words cs of
    ["noop"] -> x : loop x css
    ["addx",arg] -> let x1 = x + read arg in x : x1 : loop x1 css
    _ -> error "never happen"
loop x [] = [x]

test1 = loop 1 ["noop","addx 3", "addx -5"]

body1 fn = do
  ls <- lines <$> readFile fn
  let xs = 0 : loop 1 ls
  print $ sum $ map (\i -> i * xs !! pred i) [20,60..length xs]

test2 = body1 "test.txt"

main1 = body1 "input.txt"

{-
1始まりは辛いので、時刻0始まりで考える。
レジスタXの初期値も0で考えるとちょうどいい。

CRTのスキャンが40サイクルで6列をなす。mod 40 ということ。
レジスタXの値±1がちょうどそのmod 40に入っているとき、そのピクセルがONになる。

うーん？CPUがレジスタの値を更新するのはクロック終了の瞬間で、
CRTの方はクロック突入のときの値で表示を考えると。
なんだかズレてややこしいなぁ。

ともかく、レジスタXの値の系列のそれぞれの位置が時刻と対応していて、
その時刻は画面の特定のピクセルと対応しているから、
一発で表示の内容は決定される。
-}

body2 fn = do
  ls <- lines <$> readFile fn
  let xs = 0 : loop 1 ls
  let ps = zipWith sprite [0..] xs
  mapM_ putStrLn $ chunksOf 40 ps

sprite t x
  | pred x <= tm && tm <= succ x = '#'
  | otherwise = ' '
  where
    tm = mod t 40

test3 = body2 "test.txt"
main2 = body2 "input.txt"
