import Data.Char
-- import Control.Monad

main = do
  fi <- readFile "input.txt"
  let before = filter isAlpha fi
  let after = react before
  print (length after)
  putStrLn "part2"
  let part2ans = part2 after
  print part2ans
  print $ minimum part2ans

{-
すごい遅い。毎回包むとか辛いわ。

step :: String -> Maybe String
step (a:b:cs)
  | toUpper a == toUpper b && isLower a /= isLower b = Just cs
  | otherwise = step (b:cs) >>= return . (a :)
step _ = Nothing

react :: String -> String
react cs = case step cs of
  Just cs1 -> react cs1
  Nothing -> cs

合流すると信じて、一気に処理するようにしよう。
-}

test1 = "dabAcCaCBAcCcaDA"

react :: String -> String
react cs = loop False "" cs

loop :: Bool -> String -> String -> String
loop flg ys (a:b:cs)
  | toUpper a == toUpper b && isLower a /= isLower b = loop True ys cs
  | otherwise = loop flg (a:ys) (b:cs)
loop flg ys [a] = loop flg (a:ys) ""
loop True ys "" = loop False "" ys
loop False ys "" = ys

{-
後半はさらに時間かかるなこれ。

A..Zを取り除いたものに馬鹿真面目にやってみるしかないのか？
それとも、前半の結果をスタート地点にしていいか？
後者で充分だな。そうか。
-}

part2 cs =
  [ (length after2, x)
  | x <- ['A'..'Z']
  , let cs1 = filter ((x /=).toUpper) cs
  , let after2 = react cs1
  ]

{-
*Main> main
10766
part2
[(10360,'A'),(10306,'B'),(10330,'C'),(10288,'D'),(10360,'E'),(10376,'F'),(10356,'G'),(10360,'H'),(10396,'I'),(10350,'J'),(10314,'K'),(10354,'L'),(10326,'M'),(10390,'N'),(10344,'O'),(6538,'P'),(10330,'Q'),(10320,'R'),(10356,'S'),(10262,'T'),(10300,'U'),(10316,'V'),(10328,'W'),(10336,'X'),(10332,'Y'),(10348,'Z')]
(6538,'P')

賢くやるようなアルゴリズム、ないよねこれ？
並列計算に分けるぐらいのことはできるかもしらんけど。
-}
