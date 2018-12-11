{-# Language Strict #-}

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ncode = sum $ map length ls
  let nmem = sum $ map count ls
  print (ncode - nmem)
  let ndcode = sum $ map encCount ls
  print (ndcode - ncode)

-- メモリ中の文字数を数える。
count :: String -> Int
count cs = loop 0 (tail cs) where
  loop k ('"':[]) = k
  loop k ('\\':'\\':cs) = loop (succ k) cs
  loop k ('\\':'"':cs) = loop (succ k) cs
  loop k ('\\':'x':a:b:cs) = loop (succ k) cs
  loop k (_:cs) = loop (succ k) cs

-- もう一度エンコードした文字数を数える。
encCount :: String -> Int
encCount cs = 2 + sum (map len cs) where
  len '"' = 2
  len '\\' = 2
  len _ = 1

{-
*Main> main
1371
2117
-}
