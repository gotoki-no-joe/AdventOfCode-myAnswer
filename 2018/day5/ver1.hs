import Data.Char
-- import Control.Monad

main = do
  fi <- readFile "input.txt"
  let before = init fi
  let after = react3 before
  print (length after)
  putStrLn "part2"
  let part2ans = part2 after
  print part2ans
  print $ minimum part2ans

-- 反応するか
pair :: Char -> Char -> Bool
pair a b =
  if isLower a then  toUpper a == b
  else {-isUpper a-} toLower a == b

pair1 a b =
  if isLower a then  isUpper b && a == toLower b
  else {-isUpper a-} isLower b && toLower a == b

pair0 a b = toUpper a == toUpper b && isLower a /= isLower b

-- 1箇所ずつ反応させる
-- 反応が起きたかどうかをMaybeで返す
step :: String -> Maybe String
step (a:b:cs)
  | pair a b  = Just cs
  | otherwise = do
      res <- step (b:cs)
      return (a : res)
step _ = Nothing

-- 反応か起きなくなるまで繰り返す
react :: String -> String
react cs = case step cs of
  Just cs1 -> react cs1
  Nothing  -> cs

-- 結果：すごく遅い。

-- 反応できるところは全て反応させる
-- 一度でも反応が起きたかどうかをBoolで返す
step1 :: String -> (String,Bool)
step1 (a:b:cs)
  | pair a b  = let (s,_) = step1 cs in (s, True)
  | otherwise = let (s,f) = step1 (b:cs) in (a:s,f)
step1 cs = (cs,False)

-- 反応か起きなくなるまで繰り返す
react1 :: String -> String
react1 cs = case step1 cs of
  (cs1, True) -> react1 cs1
  _           -> cs

-- 結果：ずっと速い。

test1 = "dabAcCaCBAcCcaDA"

-- reverseしても結果は変わらないので、反復的にやる
loop :: Bool -> String -> String -> String
loop flg ys (a:b:cs)
  | pair a b  = loop True ys cs
  | otherwise = loop flg (a:ys) (b:cs)
loop flg ys [a] = loop flg (a:ys) ""
loop True ys "" = loop False "" ys
loop False ys "" = ys

react2 :: String -> String
react2 cs = loop False "" cs

-- 結果：同じぐらい速い。

{-
part2

A..Zをそれぞれ取り除いたものを反応させ、結果の長さを見る。
時間の節約のために、part1の結果を出発点にする。
-}

part2 cs =
  [ (length after2, x)
  | x <- ['A'..'Z']
  , let cs1 = filter ((x /=).toUpper) cs
  , let after2 = react3 cs1
  ]

{-
*Main> main
10766
part2
[(10360,'A'),(10306,'B'),(10330,'C'),(10288,'D'),(10360,'E'),(10376,'F'),(10356,'G'),(10360,'H'),(10396,'I'),(10350,'J'),(10314,'K'),(10354,'L'),(10326,'M'),(10390,'N'),(10344,'O'),(6538,'P'),(10330,'Q'),(10320,'R'),(10356,'S'),(10262,'T'),(10300,'U'),(10316,'V'),(10328,'W'),(10336,'X'),(10332,'Y'),(10348,'Z')]
(6538,'P')
力づくでない賢いアルゴリズムは存在するのだろうか。
-}

-- 反応して新たにつながった両端が反応するかどうかをまず調べる、loopの変形を考えてみよう。

loop1 :: Bool -> String -> String -> String
loop1 flg (a:ys) (b:cs)
  | pair a b  = loop1 True ys cs
  | otherwise = loop1 flg (b:a:ys) cs
loop1 flg [] (b:cs) = loop1 flg [b] cs
loop1 True  ys "" = loop1 False "" ys
loop1 False ys "" = ys

react3 :: String -> String
react3 cs = loop1 False "" cs

-- 結果：これが正解でした。