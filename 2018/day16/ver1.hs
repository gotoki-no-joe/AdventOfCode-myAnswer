import Data.List
import Data.Bits
import qualified Data.IntMap as M

{-
なかなか入力がでかいのですが、
切れ目はどうなっていますか？
3行の空白。
ともかく読み込もう。
-}

main = do
  fi <- readFile "input.txt"
  let (behs, prog) = parse $ lines fi
  let cnts = compute1 behs
  print $ length $ filter (3 <=) cnts
--  print $ map (\xs -> (head xs, length xs)) $ group $ sort cnts
  let posm = possibles behs
  fixed <- refine posm
  print fixed
  let result = runTestProg fixed prog
  print result

type Regs = [Int] -- of length 4
type Inst = (Int,Int,Int,Int) -- opcode, a, b, c

parse :: [String] -> ([(Regs,Inst,Regs)],[Inst])
parse ls = (gehs, prog) where
  gehs = parseGehs ls
  prog = parseProg (drop (4 * length gehs + 2) ls)
  parseGehs (bef:inst:aft:"":ls)
    | isPrefixOf "Before" bef = ( read $ drop 8 bef
                                , l2t4 $ map read $ words inst
                                , read $ drop 8 aft ) : parseGehs ls
  parseGehs _ = []
  parseProg = map (l2t4 . map read . words)

l2t4 [o,a,b,c] = (o,a,b,c)

-- レジスタ更新のレンズ的な

setc 0 v (_:rs) = v:rs
setc c v (r:rs) = r : setc (pred c) v rs

-- 各命令16個の動作を実装する

{-
addr a b c regs = setc c (regs !! a + regs !! b) regs
addi a b c regs = setc c (regs !! a + b) regs
mulr a b c regs = setc c (regs !! a * regs !! b) regs
muli a b c regs = setc c (regs !! a * b) regs
banr a b c regs = setc c (regs !! a .&. regs !! b) regs
bani a b c regs = setc c (regs !! a .&. b) regs
borr a b c regs = setc c (regs !! a .|. regs !! b) regs
bori a b c regs = setc c (regs !! a .|. b) regs
setr a b c regs = setc c (regs !! a) regs
seti a b c regs = setc c (a) regs
gtir a b c regs = setc c (if a > regs !! b then 1 else 0) regs
gtri a b c regs = setc c (if regs !! a > b then 1 else 0) regs
gtrr a b c regs = setc c (if regs !! a > regs !! b then 1 else 0) regs
eqir a b c regs = setc c (if a == regs !! b then 1 else 0) regs
eqri a b c regs = setc c (if regs !! a == b then 1 else 0) regs
eqrr a b c regs = setc c (if regs !! a == regs !! b then 1 else 0) regs

間違ってはいないがダサいなぁ。
-}

inst :: Bool -> Bool -> (Int -> Int -> Int) ->
        (Int -> Int -> Int -> [Int] -> [Int])
inst o1 o2 f = \ a b c regs ->
  let
    v1 = if o1 then regs !! a else a
    v2 = if o2 then regs !! b else b
  in
    setc c (f v1 v2) regs

addr = inst True True  (+)
addi = inst True False (+)
mulr = inst True True  (*)
muli = inst True False (*)
banr = inst True True  (.&.)
bani = inst True False (.&.)
borr = inst True True  (.|.)
bori = inst True False (.|.)
setr = inst True  undefined const
seti = inst False undefined const
gtir = inst False True sgt
gtri = inst True False sgt
gtrr = inst True True  sgt
eqir = inst False True stq
eqri = inst True False stq
eqrr = inst True True  stq
sgt a b = if a > b then 1 else 0
stq a b = if a ==b then 1 else 0

insts = [ addr, addi, mulr, muli, banr, bani, borr, bori
        , setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr ]


-- 全振る舞いについて、可能な命令がいくつか数える
compute1 :: [(Regs,Inst,Regs)] -> [Int]
compute1 behs = map try behs where
  try (regB,(o,a,b,c),regA) = length
    [ ()
    | inst <- insts
    , let reg = inst a b c regB
    , reg == regA
    ]

-- テスト
test0 = map (\f -> f 2 1 2 [3,2,1,1]) [mulr, addi, seti]
test1 = compute1 [([3,2,1,1],(9,2,1,2),[3,2,2,1])]

{-
後半、すげぇ話が飛ぶのな。
同定するところを一気にやれと。
まぁこのフォーマットじゃそうか。

振る舞いサンプルからは、opcode -> 可能性のある命令集合 が得られる。
同じコードの違うサンプルからは、共通部分をとることができる。
singletonになった命令は確定。

もしこれで絞りきれない場合、
確定された命令をそうでないコードの可能性から引く。
singletonになった命令は確定。
を、全部確定するまで繰り返す感じか。
-}

-- instsのindex を実体命令番号と呼ぶことにする。
-- オペコードに対して、可能な実体命令番号のリストのマップを作る
possibles :: [(Regs,Inst,Regs)] -> M.IntMap [Int]
possibles behs = M.fromListWith intersect assocs where
  assocs = map try behs
  try (regB,(o,a,b,c),regA) = (o,
    [ code
    | (inst,code) <- zip insts [0..]
    , let reg = inst a b c regB
    , reg == regA
    ])

-- 絞り込みを全部表示して楽しもう。
-- 全てsingletonなら終わり。
-- さもなくば、そうである実体命令番号のリストを作り、
-- singletonでない要素からそれを引くM.map を行い再帰
refine :: M.IntMap [Int] -> IO (M.IntMap [Int])
refine m = do
  let elemsm = M.elems m
  print m
  if all singleton elemsm then return m else do
    let fixed = concat $ filter singleton elemsm
    refine (M.map (killorsave fixed) m)

singleton [_] = True
singleton _ = False

killorsave fixed pos
  | singleton pos = pos
  | otherwise     = pos \\ fixed

-- 13回の絞り込みでちゃんとfixしたですよ。すごい。
-- あとは、対応を使って初期状態からエミュレーションするだけ。
-- mapのrangeにあるsingletonの中身を、対応する関数に置き換えて実行。
runTestProg :: M.IntMap [Int] -> [Inst] -> Regs
runTestProg m prog = foldl step [0,0,0,0] prog where
  step regs (o,a,b,c) = (insts !! head (m M.! o)) a b c regs
-- 効率的にはopcodeからinstsの中身へのIntMapを作るべきだが、もういいや。

{-
*Main> main
547
fromList [(0,[14]),(1,[4,10,11,12,13,14,15]),(2,[1,2,4,6,7,8,9,11,13,15]),(3,[2,4,5,9,10,11,14,15]),(4,[4,5,9,10,11,12,13,14,15]),(5,[4,5,10,11,12,13,14,15]),(6,[0,1,2,3,4,5,6,7,8,9,10,11,12]),(7,[12,14]),(8,[8,10]),(9,[1,9]),(10,[10,11,12,13,14,15]),(11,[2,4,5,6,8,9,13,15]),(12,[0,1,9]),(13,[13,15]),(14,[11,12]),(15,[11,12,13,14])]
fromList [(0,[14]),(1,[4,10,11,12,13,15]),(2,[1,2,4,6,7,8,9,11,13,15]),(3,[2,4,5,9,10,11,15]),(4,[4,5,9,10,11,12,13,15]),(5,[4,5,10,11,12,13,15]),(6,[0,1,2,3,4,5,6,7,8,9,10,11,12]),(7,[12]),(8,[8,10]),(9,[1,9]),(10,[10,11,12,13,15]),(11,[2,4,5,6,8,9,13,15]),(12,[0,1,9]),(13,[13,15]),(14,[11,12]),(15,[11,12,13])]
fromList [(0,[14]),(1,[4,10,11,13,15]),(2,[1,2,4,6,7,8,9,11,13,15]),(3,[2,4,5,9,10,11,15]),(4,[4,5,9,10,11,13,15]),(5,[4,5,10,11,13,15]),(6,[0,1,2,3,4,5,6,7,8,9,10,11]),(7,[12]),(8,[8,10]),(9,[1,9]),(10,[10,11,13,15]),(11,[2,4,5,6,8,9,13,15]),(12,[0,1,9]),(13,[13,15]),(14,[11]),(15,[11,13])]
fromList [(0,[14]),(1,[4,10,13,15]),(2,[1,2,4,6,7,8,9,13,15]),(3,[2,4,5,9,10,15]),(4,[4,5,9,10,13,15]),(5,[4,5,10,13,15]),(6,[0,1,2,3,4,5,6,7,8,9,10]),(7,[12]),(8,[8,10]),(9,[1,9]),(10,[10,13,15]),(11,[2,4,5,6,8,9,13,15]),(12,[0,1,9]),(13,[13,15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4,10,15]),(2,[1,2,4,6,7,8,9,15]),(3,[2,4,5,9,10,15]),(4,[4,5,9,10,15]),(5,[4,5,10,15]),(6,[0,1,2,3,4,5,6,7,8,9,10]),(7,[12]),(8,[8,10]),(9,[1,9]),(10,[10,15]),(11,[2,4,5,6,8,9,15]),(12,[0,1,9]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4,10]),(2,[1,2,4,6,7,8,9]),(3,[2,4,5,9,10]),(4,[4,5,9,10]),(5,[4,5,10]),(6,[0,1,2,3,4,5,6,7,8,9,10]),(7,[12]),(8,[8,10]),(9,[1,9]),(10,[10]),(11,[2,4,5,6,8,9]),(12,[0,1,9]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[1,2,4,6,7,8,9]),(3,[2,4,5,9]),(4,[4,5,9]),(5,[4,5]),(6,[0,1,2,3,4,5,6,7,8,9]),(7,[12]),(8,[8]),(9,[1,9]),(10,[10]),(11,[2,4,5,6,8,9]),(12,[0,1,9]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[1,2,6,7,9]),(3,[2,5,9]),(4,[5,9]),(5,[5]),(6,[0,1,2,3,5,6,7,9]),(7,[12]),(8,[8]),(9,[1,9]),(10,[10]),(11,[2,5,6,9]),(12,[0,1,9]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[1,2,6,7,9]),(3,[2,9]),(4,[9]),(5,[5]),(6,[0,1,2,3,6,7,9]),(7,[12]),(8,[8]),(9,[1,9]),(10,[10]),(11,[2,6,9]),(12,[0,1,9]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[1,2,6,7]),(3,[2]),(4,[9]),(5,[5]),(6,[0,1,2,3,6,7]),(7,[12]),(8,[8]),(9,[1]),(10,[10]),(11,[2,6]),(12,[0,1]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[6,7]),(3,[2]),(4,[9]),(5,[5]),(6,[0,3,6,7]),(7,[12]),(8,[8]),(9,[1]),(10,[10]),(11,[6]),(12,[0]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[7]),(3,[2]),(4,[9]),(5,[5]),(6,[3,7]),(7,[12]),(8,[8]),(9,[1]),(10,[10]),(11,[6]),(12,[0]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[7]),(3,[2]),(4,[9]),(5,[5]),(6,[3]),(7,[12]),(8,[8]),(9,[1]),(10,[10]),(11,[6]),(12,[0]),(13,[15]),(14,[11]),(15,[13])]
fromList [(0,[14]),(1,[4]),(2,[7]),(3,[2]),(4,[9]),(5,[5]),(6,[3]),(7,[12]),(8,[8]),(9,[1]),(10,[10]),(11,[6]),(12,[0]),(13,[15]),(14,[11]),(15,[13])]
[582,6,582,2]
-}
