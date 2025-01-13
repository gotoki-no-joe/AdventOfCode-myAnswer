{-
静的な情報として、ユニットの名前に対応づけて、
・それがFFなのかConjなのかの区別
・その出力を送り出す先
というグラフが必要。

動作として、FFは、
・high pulseでは何もしない
・low pulseでは、自分の状態を反転して、対応したpulseを出す
だけなので、staticな接続元は不要。dynamicな現在の状態は必要。1ビットだけ。

Conjは、
・接続元の最新の出力がオールhighならlow
・一つでもlowならhigh
とやるので、
(A)全ての素子の最終出力を持っておき、それを参照できる接続も保持する
(B)素子のdynamicな状態として、元全員の記録を持っておく（定義どおり）
のいずれかをすることになる。Bでいくか普通に。Map String Bool な感じ。
もっとアレなら、最終がlowな親の名前のリスト、にすれば、nullならhighとできる。
初期値を作るのが少し面倒かもだが。

dynamicな情報を統一的に一つのmapで持つか、型が違うので二つのmapにするか。
両者を統合するデータ型を作って、staticな方とdynamicな方の両方を管理するとかっこいいかな。
そういう工夫がpart2でぶち壊しになりそうなのがAoCの嫌なところだけどね。
-}
import Debug.Trace

import qualified Data.Map as M
import Data.List.Split

-- 入力元
data Module = BCmodule | FFmodule | ConjModule [String] deriving Show
-- type Module = (String, [String], [String])
-- 出力先
type ModuleP = ([String], Module)

part12 fn body = do
  ls <- lines <$> readFile fn
  print $ body ls

parse1 l = (lhs, outs)
  where
    [lhs, rhs] = splitOn " -> " l
    outs = wordsBy (flip elem ", ") rhs

solve1 ls = (highs, lows, highs * lows, rxH, rxL)
  where
    lrs = map parse1 ls
    inedges = M.fromListWith (++) [(o, [tail lhs]) | (lhs, outs) <- lrs, o <- outs]
    nw = {- M.insert "output" ([], BCmodule) $ -} M.fromList $ map nwf lrs
    nwf (bc@"broadcaster", outs) = (bc, (outs, BCmodule))
    nwf ('%':name, outs) = (name, (outs, FFmodule))
    nwf ('&':name, outs) = (name, (outs, ConjModule $ inedges M.! name))

    inits = M.map (initsf . snd) nw
    initsf BCmodule = BCstate
    initsf FFmodule = FFstate False
    initsf (ConjModule ies) = ConjState ies

    signals = loop 1000 0 inits signals
    loop 0 0 _ _ = []
    loop times 0 state sigs = ("broadcaster", "button", False) : loop (pred times) 1 state sigs
    loop times len state ((mod, src, hl):sigs) -- = trace (show mod) $
      | M.notMember mod nw = loop times (pred len) state sigs -- outputだのrxだの
      | otherwise =
      case (modEnt, state M.! mod) of
        (BCmodule, _) -> sub outs hl state
        (FFmodule, FFstate b) | hl -> sub [] hl state
          | otherwise -> sub outs (not b) (M.insert mod (FFstate $ not b) state)
        (ConjModule ins, ConjState lows) | hl -> conjSub $ filter (src /=) lows
          | otherwise -> conjSub $ src : lows
        _ -> []
      where
        (outs, modEnt) = nw M.! mod
        sub os v st = [(o, mod, v) | o <- os] ++ loop times (pred len + length os) st sigs
        conjSub [] = sub outs False (M.insert mod (ConjState []) state)
        conjSub ls = sub outs True  (M.insert mod (ConjState ls) state)
    
    highs = length [() | (_,_,True) <- signals]
    lows = length [() | (_,_,False) <- signals]
    rxL = length [() | ("rx",_,False) <- signals]
    rxH = length [() | ("rx",_,True) <- signals]

-- FF : high/low, Conj : 最後がlowだった親のリスト
data State = BCstate | FFstate Bool | ConjState [String] deriving Show

{-
ghci> part12 "samp1.txt" solve1
(4000,8000,32000000)
ghci> part12 "samp2.txt" solve1
(2750,4250,11687500)
ghci> part12 "input.txt" solve1
(50633,19364,980457412)
-}

solve2 ls = length [() | (_,"button",_) <- signals]
  where
    lrs = map parse1 ls
    inedges = M.fromListWith (++) [(o, [tail lhs]) | (lhs, outs) <- lrs, o <- outs]
    nw = {- M.insert "output" ([], BCmodule) $ -} M.fromList $ map nwf lrs
    nwf (bc@"broadcaster", outs) = (bc, (outs, BCmodule))
    nwf ('%':name, outs) = (name, (outs, FFmodule))
    nwf ('&':name, outs) = (name, (outs, ConjModule $ inedges M.! name))

    inits = M.map (initsf . snd) nw
    initsf BCmodule = BCstate
    initsf FFmodule = FFstate False
    initsf (ConjModule ies) = ConjState ies

    signals = loop 0 inits signals
    loop 0 state sigs = ("broadcaster", "button", False) : loop 1 state sigs
    loop len state ((mod, src, hl):sigs) -- = trace (show mod) $
      | mod == "rx", hl = sub [] False state
      | mod == "rx" = [] -- 出た！ここで止めていい。
      | otherwise =
      case (modEnt, state M.! mod) of
        (BCmodule, _) -> sub outs hl state
        (FFmodule, FFstate b) | hl -> sub [] hl state
          | otherwise -> sub outs (not b) (M.insert mod (FFstate $ not b) state)
        (ConjModule ins, ConjState lows) | hl -> conjSub $ filter (src /=) lows
          | otherwise -> conjSub $ src : lows
        _ -> []
      where
        (outs, modEnt) = nw M.! mod
        sub os v st = [(o, mod, v) | o <- os] ++ loop (pred len + length os) st sigs
        conjSub [] = sub outs False (M.insert mod (ConjState []) state)
        conjSub ls = sub outs True  (M.insert mod (ConjState ls) state)

{-
CPUがブン回るばかりで終わらない。そうきたか…
ちなみに1000回押すpart1の結果を見ると、
ghci> part12 "input.txt" solve1
(50633,19364,980457412,6930,0)
Highはそれなりに送信されている。

何をどうやって数えるんだろうなこれ。
-}
