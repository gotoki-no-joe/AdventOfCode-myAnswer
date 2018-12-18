import qualified Data.IntMap as M
import Data.List

{-
value * goes to bot *
の行が、初期値を与える。
bot * gives low to (bot|output) * and high to (bot|output) *
の行が、ロボの動作を定義する。
-}

data Dest = Bot Int | Bin Int  -- ロボット番号と製品箱番号
  deriving (Eq, Ord, Show)

-- 命令 arg1のIntはその動作をするロボの番号
data Inst = Initial Int Int    -- value 5 goes to bot 2 は Initial 2 5
          | Give Int Dest Dest     -- Destはそれぞれ low, high
  deriving (Eq, Ord, Show)

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let insts = makeInst ls
  let acts0 = makeInit ls
  let final@(roboF,outF,_) = run insts acts0
  let ans1 = compute1 roboF
  print ans1
  let out012 = concatMap (outF M.!) [0,1,2]
  print out012
  let ans2 = product out012
  print ans2
  return ()

-- 読み取り
parse :: String -> Inst
parse ('v':cs) = Initial (read ns0) (read ns1) where
  [_,ns1,_,_,_,ns0] = words cs
parse ('b':cs) = Give (read ns0) (dest ds1 $ read ns1) (dest ds2 $ read ns2) where
  [_,ns0,_,_,_,ds1,ns1,_,_,_,ds2,ns2] = words cs
  dest "bot" = Bot
  dest "output" = Bin

-- ロボごとに命令を引けるようにする
makeInst :: [Inst] -> M.IntMap Inst
makeInst ls = M.fromList [ (id, inst) | inst@(Give id _ _) <- ls ]

-- Initialを受け渡しアクションに変換
makeInit :: [Inst] -> [(Int,Int)]
makeInit ls = [ (id, val) | Initial id val <- ls ]

-- ステップ動作
-- 受け渡しアクションの先頭を実行
-- ロボの持つチップが2つになったら、
-- 命令に従ってそれを引き渡すアクションに変換

step :: M.IntMap Inst    -- 命令
     -> (M.IntMap [Int]  -- ロボが持ったチップ
        ,M.IntMap [Int]  -- 製品箱に入ったチップ
        ,[(Int,Int)]     -- 受け渡しアクション列
        )
     -> (M.IntMap [Int]  -- 更新後の状態
        ,M.IntMap [Int]
        ,[(Int,Int)]
        )
step insts (robo,outp,(id,val):acts)
  | not $ len2 (robo1 M.! id) = (robo1,outp,acts)
  | True = give d2 vH $ give d1 vL $ (robo1,outp,acts)
  where
    robo1 = M.insertWith (++) id [val] robo
    [vL,vH] = sort (robo1 M.! id)
    Give _ d1 d2 = insts M.! id
    give (Bot des) val (robo,outp,acts) = (robo,outp,(des,val):acts)
    give (Bin des) val (robo,outp,acts) = (robo, M.insertWith (++) des [val] outp, acts)

len2 (_:_:_:_) = error "too many"
len2 [_,_] = True
len2 _ = False

-- アクション列が空になるまでステップを回す
run insts acts0 = loop (M.empty, M.empty, acts0) where
  loop final@(_,_,[]) = final
  loop triple = loop $ step insts triple

-- 61と17を比較したロボを見つける
compute1 robo = M.filter (\l -> elem 61 l && elem 17 l) robo
