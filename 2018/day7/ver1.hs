import Data.List
import Data.Char (ord)

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ps = map parse ls
--  print ps
  let steps = map head $ group $ sort (map fst ps ++ map snd ps)
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
parse cs = (head $ drop 5 cs, head $ drop 36 cs)

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
    avail =
      [ s
      | s <- steps
      , null $ filter ((s ==).snd) pairs
      ]
    s = head avail
    steps1 = delete s steps
    pairs1 = filter ((s /=).fst) pairs

{-
今度はどうすればいいんだ。

毎秒ごとの状況をシミュレーションする。
・ワーカーは、仕事を持っていれば1つ減らす。ちょうど完了するなら申告する。
・完了された仕事があったら、制約リストから削除する。
・暇なワーカーと割り当てられる仕事の両方があったら、
　可能なかぎりそれを割り当てる。
　割り当て済みかつ未完了な仕事を把握するために、(x,x)という制約を追加する。(hack!)
  いや、stepsから削除すれば検索対象になりませんから大丈夫。
・手順がなくなってワーカーが全員暇になったら完了！
-}

-- phase1

simulate :: String          -- 残りの仕事
         -> [(Char, Int)]   -- ワーカーの状態
         -> [(Char,Char)]   -- 制約
         -> [[(Char, Int)]] -- 毎秒の状況

simulate "" [] [] = []
simulate steps ws pairs = phase2 steps ws1 pairs1 where
  comps = [ c | (c,1) <- ws ]
  ws1 = [ (c,pred n) | (c,n) <- ws, n > 1 ]
  pairs1 = filter (flip notElem comps.fst) pairs
--  steps1 = filter (flip notElem comps) steps

len5 [_,_,_,_,_] = True
len5 _ = False

phase2 steps ws pairs
  | len5 ws    = ws : simulate steps ws pairs
  | null avail = ws : simulate steps ws pairs
  | otherwise  = phase2 steps1 ((s, 61 + ord s - ord 'A'):ws) pairs
  where
    avail =
      [ s
      | s <- steps
      , null $ filter ((s ==).snd) pairs
      ]
    s = head avail
    steps1 = delete s steps

-- 防止したはずなのに、着手済みの仕事をみんな重ねて引き受けてしまう。
-- いやそんなことない。正しい結果だ多分。

{-
> main
"EPWCFXKISTZVJHDGNABLQYMORU compulete!"
part2
952
-}
