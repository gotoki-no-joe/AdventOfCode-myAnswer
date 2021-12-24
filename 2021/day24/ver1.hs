import Data.List

type Reg = (Int,Int,Int,Int)
type State =
  ( [String] -- プログラム
  , Reg -- w,x,y,zレジスタ
  , [Int]) -- 入力の逆順
type Result = ([Int],Reg)

-- 素朴なインタプリタでなくて、inp命令で9分岐する、深さ優先探索をするemulatorが必要だ。

step :: State -> ([Result], [State])
step ([], wxyz, is) = ([(is, wxyz)],[])
step (l:ls, (w,x,y,z), is) =
  case words l of
    ["inp", a] -> ([], [(ls, write a i, i:is) | i <- [9,8..1]])
    ["add", a, b] -> ([], [(ls, write a (rread a + rread b), is)])
    ["mul", a, b] -> ([], [(ls, write a (rread a * rread b), is)])
    ["div", a, b] -> let bv = rread b in if bv == 0 then ([],[]) else ([], [(ls, write a (rread a `div` bv), is)])
    ["mod", a, b] -> let av = rread a; bv = rread b in if av < 0 || bv <= 0 then ([],[]) else ([], [(ls, write a (av `mod` bv), is)])
    ["eql", a, b] -> let av = rread a; bv = rread b in ([], [(ls, write a $ if av == bv then 1 else 0, is)])
    _ -> error ("never happens" ++ l)
  where
    rread "w" = w
    rread "x" = x
    rread "y" = y
    rread "z" = z
    rread xs = read xs
    write "w" v = (v,x,y,z)
    write "x" v = (w,v,y,z)
    write "y" v = (w,x,v,z)
    write "z" v = (w,x,y,v)
    write _ _ = error "never happens"

-- @gotoki_no_joe
dfs :: (s -> ([x],[s])) -> s -> [x]
dfs f i = loop [i]
  where
    loop [] = []
    loop (x:xs) = let (a,b) = f x in a ++ loop (b ++ xs)

main1 fn = do
  p <- readFile fn
  print $ dfs step (lines p, (0,0,0,0), [])

test1 = main1 "sample1.txt"

run1 = do
  p <- readFile "input.txt"
  print $ reverse $ fst $ head $ filter valid $ dfs step (lines p, (0,0,0,0), [])

valid (_,(_,_,_,z)) = z == 0

type State2 = ([(Int,Int,Int)], Int, [Int]) -- Prog, z, is
type Result2 = ([Int],Int)
step2 :: State2 -> ([Result2],[State2])
step2 ([],z,is) = ([(is,z)],[])
step2 ((p1,p2,p3):ps,z,is) = ([], map f [9,8..1])
  where
    f w = (ps,z1,w:is)
      where
        x  = if mod z 26 + p1 /= w then 1 else 0
        z1 = div z p2 * (x * 25 + 1) + (w + p3) * x

prog = zip3 [13,15,15,11,-7,10,10,-5,15,-3,0,-5,-9,0] [1,1,1,1,26,1,1,26,1,26,26,26,26,26] [6,7,10,2,15,8,1,10,5,3,5,11,12,10]

run12 = reverse $ fst $ head $ filter ((0 ==) . snd) $ dfs step2 (prog, 0, [])

-------

type State3 = ([(Int,Int,Int)], [Int], [Int]) -- prog, zs, is
type Result3 = ([Int],[Int]) -- is, zs
step3 :: State3 -> ([Result3],[State3])
step3 ([],zs,is) = ([(is,zs)],[])
step3 ((p1,p2,p3):ps,zs,is) = ([], if p2 == 1 then map f1 [9,8..1] else f26)
  where
    f1 w = (ps, w + p3 : zs, w : is)
    f26 = [(ps, tail zs, w : is) | let w = head zs + p1, 0 < w, w < 10]

run13 = reverse $ fst $ head $ filter (null . snd) $ dfs step3 (prog, [], [])

{-
*Main> run13
[3,9,4,9,4,1,9,5,7,9,9,9,7,9]

39494195799979
-}

step4 :: State3 -> ([Result3],[State3])
step4 ([],zs,is) = ([(is,zs)],[])
step4 ((p1,p2,p3):ps,zs,is) = ([], if p2 == 1 then map f1 [1..9] else f26)
  where
    f1 w = (ps, w + p3 : zs, w : is)
    f26 = [(ps, tail zs, w : is) | let w = head zs + p1, 0 < w, w < 10]

run2 = reverse $ fst $ head $ filter (null . snd) $ dfs step4 (prog, [], [])

{-
*Main> run2
[1,3,1,6,1,1,5,1,1,3,9,6,1,7]

13161151139617

14文字だし、26進数だし、
merrychristmas
と関係あると思ったのだけどなぁ。
-}
