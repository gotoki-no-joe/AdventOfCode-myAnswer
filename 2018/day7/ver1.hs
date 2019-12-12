import Data.List
import Data.Char (ord)

main = do
  fi <- readFile "input.txt"
  let ps = map parse $ lines fi
  let steps = map head $ group $ sort (map fst ps ++ map snd ps) -- faster than nub
--  print steps -- is ['A'..'Z']
  let ans1 = compute1 steps ps
  print ans1
  putStrLn "part2"
  let seq = simulate steps [] ps
--  print seq
  print (length seq - 1)

-- Step C must be finished before step A can begin.
-- .....6...|.........|.........|......7
parse :: String -> (Char,Char)
parse cs = (cs !! 5, cs !! 36)

{-
stepsが空になったら完了
stepsの中で、制約のない先頭を見つけてそれを実行
それを条件にしている制約を除去し、stepsからも削除し、再帰
-}

compute1 :: String -> [(Char,Char)] -> String
compute1 "" _ = " compulete!"
compute1 steps pairs
  | null avail =" Oops! failed with:" ++ steps
  | otherwise  = s : compute1 steps1 pairs1
  where
    avail = [s | s <- steps, null $ filter ((s ==).snd) pairs]
    s = head avail
    steps1 = delete s steps
    pairs1 = filter ((s /=).fst) pairs

{-
毎秒ごとの状況をシミュレーションする。
・ワークは、残り時間が減っていく。
・ちょうど完了した仕事について、制約リストからそれを削除する。
・暇なワーカーと割り当てられる仕事の両方があったら、可能なかぎりそれを割り当てる。
・手順がなくなってワークもなくなったら完了
-}

-- phase1 : ワークを進行させ、完了させる

simulate :: String          -- 残りの仕事
         -> [(Char, Int)]   -- 着手しているワークの状態(記号と残り時間)
         -> [(Char,Char)]   -- 制約
         -> [[(Char, Int)]] -- 毎秒の作業状況

simulate "" [] _ = []
simulate steps ws pairs = phase2 steps ws1 pairs1 where
  comps = [ c | (c,1) <- ws ]               -- 完了するワークの記号リスト
  ws1 = [ (c,pred n) | (c,n) <- ws, n > 1 ] -- 各ワークの残り時間を減らす。完了したワークは消える
  pairs1 = filter (flip notElem comps.fst) pairs -- 完了したものが塞いでいた制約を消去

len5 [_,_,_,_,_] = True
len5 _ = False

-- phase2 : ワークを新規に割り当てる

phase2 steps ws pairs
  | len5 ws    = ws : simulate steps ws pairs -- 全員作業中なら変化なし
  | null avail = ws : simulate steps ws pairs -- 取り掛かれる仕事がないなら変化なし
  | otherwise  = phase2 steps1 ((s, 61 + ord s - ord 'A'):ws) pairs -- 新たなワークを1つ起動して再度チェック
  where
    avail = [s | s <- steps, null $ filter ((s ==).snd) pairs]
    s = head avail
    steps1 = delete s steps

{-
> main
"EPWCFXKISTZVJHDGNABLQYMORU compulete!"
part2
952
-}
