-- 2022-11-30

{-
キーパッドをグラフと考えて、ノードを現在の文字で、隣接ノードへの辺を方向の文字でラベルする。
密なので、隣接行列的なマップで表し、辺がないときは空欄にする感じかな。
-}

import qualified Data.Map as M

keypad = M.fromList $
  [t | (v,w) <- zip ['1','2','4','5','7','8'] ['2','3','5','6','8','9'], t <- [((v,'R'),w), ((w,'L'),v)]] ++
  [t | (v,w) <- zip ['1','2','3','4','5','6'] ['4','5','6','7','8','9'], t <- [((v,'D'),w), ((w,'U'),v)]]

{-
方向文字列とスタート文字をとり、辿り尽くした最後の文字を返す、辿れないとき無視する
-}

exec kp c ds = foldl step c ds
  where
    step c d = M.findWithDefault c (c,d) kp

main1 = do
  co <- readFile "input.txt"
  putStrLn $ tail $ scanl (exec keypad ) '5' $ lines co
  putStrLn $ tail $ scanl (exec keypad2) '5' $ lines co

-- キーマップさえ取り換えればよい。

keypad2 = mkKeyPad ["  1  "," 234 ","56789"," ABC ","  D  "]

mkKeyPad :: [String] -> M.Map (Char,Char) Char
mkKeyPad xss = M.fromList $
  [t | xs <- xss, (v,w) <- zip xs (tail xs), v /= ' ', w /= ' ', t <- [((v,'R'),w),((w,'L'),v)]] ++
  [t | (xs, ys) <- zip xss (tail xss), (v,w) <- zip xs ys, v /= ' ', w /= ' ', t <- [((v,'D'),w),((w,'U'),v)]]
