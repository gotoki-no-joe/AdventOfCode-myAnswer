import qualified Data.Map as M

{-
入力ファイルに二通りの情報が入っている珍しいパターンだ。

マッチを確実にするために、左に2つ.を足したいが、
そういうことをすると番号がズレてしまって後で困る。
どうやって作るべきか。

マッチにより必ず2文字は減るので、両方の補正が必要。
そして、よく考えると、....# => # とかあったら3つ飛ぶので、
最初に補正するべきは3文字の...だ！わぁ怖い。

前後に...～...と追加する。スタート位置が-3される。
マッチにより次の世代を作る。左右2文字ずつ減る。スタート位置は+2される。
さらに.を切り詰めてもいいが、大して効率の変化はないだろう。

-}

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let gen0 = (0, drop 15 $ head ls)
  let rule = makeRule $ map parse $ drop 2 ls
--  print gen0
--  print rule
  let gen20 = head $ drop 20 $ iterate (step rule) gen0
  let ans1 = score gen20
  print ans1
  let genFB = head $ drop 50000000000 $ iterate (step rule . trim) gen0
  let ans2 = score genFB
  print ans2

parse :: String -> (String,Char)
parse cs = (take 5 cs, last cs)

makeRule :: [(String,Char)] -> M.Map String Char
makeRule = M.fromList

step :: M.Map String Char -> (Int,String) -> (Int,String)
step rule (ofs, pots) = (ofs - 1, result) where
  result = process ("..." ++ pots ++ "...")
  process ps@(_:_:_:_:_:_) = rule M.! take 5 ps : process (tail ps)
  process _ = []

score :: (Int,String) -> Int
score (ofs, pots) = sum [ if p == '#' then o else 0 | (o,p) <- zip [ofs..] pots ]

{-
50億と言われると、トリミングしないわけにはいかないね。
ていうか、mutable bytearrayとか使わないと現実的でないかな。
-}

trim :: (Int,String) -> (Int,String)
trim (ofs, pots) = (ofs + d1, drop d1 $ take (n-d2) pots) where
  n = length pots
  d1 = length $ takeWhile ('.' ==) pots
  d2 = length $ takeWhile ('.' ==) $ reverse pots
