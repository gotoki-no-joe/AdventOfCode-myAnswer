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
    [w1,w2,op:_,w3] -> Right (init w1, w2, op, w3)
    _ -> error "never"

phase2 fn = do
  ps <- map parse . lines <$> readFile fn
  print $ compute ps

compute ps = m M.! (parent M.! "humn", "humn") -- trace (show $ M.keys m) $ (m M.! "humn" $ "")
  where
-- ノードに対して、それを参照する親ノードの名前を引くマップを作る
-- これがないと逆流させられない
    parent = M.fromList [kv | Right (w1,w2,_,w3) <- ps, kv <- [(w2,w1),(w3,w1)]]
-- マップは、ノード名、参照元ノード名の対から、その値を返す
    m = M.fromList $
      [((k, parent M.! k), x) | Left (k, x) <- ps, k /= "humn"] ++  -- 定数は上からのみ参照される
      [ kv
      | Right ("root", wL, _, wR) <- ps
      , kv <- [(("root", wL), m M.! (wR, "root")), (("root", wR), m M.! (wL, "root"))]] ++
      [ kv
      | Right (w1, wL, '+', wR) <- ps, w1 /= "root", let wP = parent M.! w1  -- w1(wP) = wL + wR
      , kv <- [((w1,wP), m M.! (wL,w1) + m M.! (wR,w1))
              ,((w1,wL), m M.! (wP,w1) - m M.! (wR,w1))        -- w1(wL) = wP - wR
              ,((w1,wR), m M.! (wP,w1) - m M.! (wL,w1))]       -- w1(wR) = wP - wL
      ] ++
      [ kv
      | Right (w1, wL, '-', wR) <- ps, w1 /= "root", let wP = parent M.! w1  -- w1(wP) = wL - wR
      , kv <- [((w1,wP), m M.! (wL,w1) - m M.! (wR,w1))
              ,((w1,wL), m M.! (wP,w1) + m M.! (wR,w1))        -- w1(wL) = wP + wR
              ,((w1,wR), m M.! (wL,w1) - m M.! (wP,w1))]       -- w1(wR) = wL - wP
      ] ++
      [ kv
      | Right (w1, wL, '*', wR) <- ps, w1 /= "root", let wP = parent M.! w1  -- w1(wP) = wL * wR
      , kv <- [((w1,wP), m M.! (wL,w1) * m M.! (wR,w1))
              ,((w1,wL), m M.! (wP,w1) `div` m M.! (wR,w1))    -- w1(wL) = wP / wR
              ,((w1,wR), m M.! (wP,w1) `div` m M.! (wL,w1))]   -- w1(wR) = wP / wL
      ] ++
      [ kv
      | Right (w1, wL, '/', wR) <- ps, w1 /= "root", let wP = parent M.! w1  -- w1(wP) = wL / wR
      , kv <- [((w1,wP), m M.! (wL,w1) `div` m M.! (wR,w1))
              ,((w1,wL), m M.! (wP,w1) * m M.! (wR,w1))        -- w1(wL) = wP * wR
              ,((w1,wR), m M.! (wL,w1) `div` m M.! (wP,w1))]   -- w1(wR) = wL / wP
      ]

{-


{-
Double版
2.32974643455e14
Int版
2 32974643455000
-- 除算は切り捨てでいいんか。
-}

-}

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"
