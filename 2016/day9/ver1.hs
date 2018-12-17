main = do
  fi <- readFile "input.txt"
  let li = head $ lines fi
  let dec = decompress li
  print (length dec)
  putStrLn "part2"
--  let dec2 = decompress2 li
--  print (length dec2)
  let ans2 = ver2len 0 li
  print ans2

decompress :: String -> String
decompress ('(':cs) = concat (replicate cnt es) ++ decompress fs where
  (n1,'x':bs) = break ('x' ==) cs
  len = read n1
  (n2,')':ds) = break (')' ==) bs
  cnt = read n2
  (es,fs) = splitAt len ds
decompress (c:cs) = c : decompress cs
decompress "" = ""

{-
データ部分よりはみ出す長さを、内側のマーカーが指定しているなんてことは
ないんだろうね？それは反則だからね。

遅延評価なら、ぶっちゃけ実際に展開してもメモリに蓄積されはしない
はずなので、やっちゃってみましょう。

-}

decompress2 :: String -> String
decompress2 ('(':cs) = concat (replicate cnt es1) ++ decompress2 fs where
  (n1,'x':bs) = break ('x' ==) cs
  len = read n1
  (n2,')':ds) = break (')' ==) bs
  cnt = read n2
  (es,fs) = splitAt len ds
  es1 = decompress2 es
decompress2 (c:cs) = c : decompress2 cs
decompress2 "" = ""

{-
と思ったが、やっぱり無理っぽい。
長さだけをStrictに計算して求めるような形式にしましょう。
-}

ver2len :: Int -> String -> Int
ver2len n ('(':cs) = (ver2len $! (n + sublen * cnt)) fs where
  (n1,'x':bs) = break ('x' ==) cs
  len = read n1
  (n2,')':ds) = break (')' ==) bs
  cnt = read n2
  (es,fs) = splitAt len ds
  sublen = ver2len 0 es
ver2len n (_:cs) = (ver2len $! (succ n)) cs
ver2len n "" = n

{-
*Main> main
97714
part2
10762972461
-}
