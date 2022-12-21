{-
遅延評価で楽をしようと大変な努力をしている。
どうやら無理だったようだ。
-}

import Debug.Trace

import qualified Data.Map as M

type Parsed = Either (String, Int) (String, String, Char, String)

parse :: String -> Parsed
parse l =
  case words l of
    [w1,w2] -> Left (init w1, read w2)
    [w1,w2,op,w3] -> Right (init w1, w2, head op, w3)
    _ -> error "never"

phase2 fn = do
  ps <- map parse . lines <$> readFile fn
  print $ compute ps

-- 方向は 0 : 順(上から下) 1 : 左下 2 : 右下 とする。

compute ps = trace (show $ M.keys m) $ m M.! ("humn", 1)
  where
    m = M.fromList $
      [((k,0), x) | Left (k, x) <- ps, k /= "humn"] ++  -- 定数は上からしか見られない
      [ kv
      | Right ("root", wL, '=', wR) <- ps -- ここだけ、右下からか左下からかが必要！
      , kv <- [(("root",1), m M.! (wR,0))
              ,(("root",2), m M.! (wL,0))]
      ] ++
      [ kv
      | Right (w1, wL, '+', wR) <- ps
      , kv <- [((w1,0), m M.! (wL,0) + m M.! (wR,0))
              ,((wL,1), m M.! (w1,1) - m M.! (wR,0))
              ,((wR,1), m M.! (w1,2) - m M.! (wL,0))]
      ] ++
      [ kv
      | Right (w1, wL, '-', wR) <- ps
      , kv <- [((w1,0), m M.! (wL,0) - m M.! (wR,0))
              ,((wL,1), m M.! (w1,1) + m M.! (wR,0))
              ,((wR,1), m M.! (wL,0) - m M.! (w1,1))]
      ] ++
      [ kv
      | Right (w1, wL, '*', wR) <- ps
      , kv <- [((w1,0), m M.! (wL,0) * m M.! (wR,0))
              ,((wL,1), m M.! (w1,1) `div` m M.! (wR,0))
              ,((wR,1), m M.! (w1,1) `div` m M.! (wL,0))]
      ] ++
      [ kv
      | Right (w1, wL, '/', wR) <- ps
      , kv <- [((w1,0), m M.! (wL,0) `div` m M.! (wR,0))
              ,((wL,1), m M.! (w1,1) * m M.! (wR,0))
              ,((wR,1), m M.! (wL,0) `div` m M.! (w1,1))]
      ]

test1 = phase2 "test.txt"
main1 = phase2 "input.txt"

{-
Double版
2.32974643455e14
Int版
2 32974643455000
-- 除算は切り捨てでいいんか。
-}
