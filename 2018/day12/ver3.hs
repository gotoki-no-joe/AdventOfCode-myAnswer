import qualified Data.Map as M

{-
入力ファイルに二通りの情報が入っている珍しいパターンだ。

....# -> # という規則がありうるので、距離3まで飛び火する。
それを確実にするには、前後に4つ空白があることを保証する必要がある。
-}

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let gen0 = (0, drop 15 $ head ls)
  let rule = makeRule $ map parse $ drop 2 ls
{-
-- 観察する
  let gen50s = take 20 $ drop 100 $ iterate (step rule) gen0
  let ofs0 = negate $ minimum $ map fst gen50s
  mapM_ (\(ofs,pots) -> putStrLn (replicate (ofs0 + ofs) ' ' ++ pots)) gen50s

100世代を超えると、一定の形であとは右にズレてくだけだとわかった。
そういうことね。
-}
-- part1
  let gen20 = head $ drop 20 $ iterate (step rule) gen0
  let ans1 = score gen20
  print ans1

{-
バカ正直に計算して求められるはずがない。
まぁ、左右をきちんとトリミングすれば不可能ではないのだけれど。
それでも線形時間が長すぎる。

  let genFB = head $ drop 50000000000 $ iterate (step rule . trim) gen0
  let ans2 = score genFB
  print ans2
-}

{-
安定する第100世代（もう探さない）の状況を取得し、
もう50000000000-100だけ右にずれた状況のスコアを求める。
-}
  let (ofs100,pots100) = head $ drop 100 $ iterate (step rule) gen0
  let ans2 = score (ofs100 - 100 + 50000000000, pots100)
  print ans2

parse :: String -> (String,Char)
parse cs = (take 5 cs, last cs)

makeRule :: [(String,Char)] -> M.Map String Char
makeRule = M.fromList

step :: M.Map String Char -> (Int,String) -> (Int,String)
step rule (ofs, pots) = (ofs - 2, result) where
  result = process ("...." ++ pots ++ "....")
  process ps@(_:_:_:_:_:_) = rule M.! take 5 ps : process (tail ps)
  process _ = []

score :: (Int,String) -> Int
score (ofs, pots) = sum [ if p == '#' then o else 0 | (o,p) <- zip [ofs..] pots ]
