{-
二つのマシンが同期通信しながら動く。

ver1の状態に入力キューとIDを加える。
二つのプロセスのペアを入れ替えながら交互に実行させる。

変化しないIDを状態に加えるのは何な気もするなぁ。ラベルなんだけども。
flipしているかそうでないかのboolさえあればよいのだけれど。

PCがプログラムを外れたとき、停止状態。(STOP)
rcv命令でキューが空のとき、待ち状態。(WAIT)
普通に実行できるとき、実行状態。(RUN/READY)
snd命令を、自分でキューに追加するかどうかがひっかかるかしら。

-}

import Data.Array
import qualified Data.Sequence as Q
import Data.Maybe

type Prog = Array Int [String]

compile :: String -> Prog
compile src = listArray (0,pred $ length ls) ls
  where
    ls = map words $ lines src

data Status = Ready | Wait | Stop deriving Eq

data State = State
  { pc :: Int
  , regs :: Array Char Int
  , q :: Q.Seq Int -- 入力キュー
  , status :: Status
  , idee :: Int}

state0 = State 0 (accumArray (+) 0 ('a','z') [])         (Q.Empty) Ready 0
state1 = State 0 (accumArray (+) 0 ('a','z') [('p',1)])  (Q.Empty) Ready 1

enQ :: State -> Int -> State
enQ st msg = st { q = q st Q.:|> msg, status = if status st == Wait then Ready else status st}

-- 送信があったらする
step :: Prog -> State -> (State, Maybe Int)
step prog st | pc st < 0 || snd (bounds prog) < pc st = (st {status = Stop}, Nothing)
step prog st =
  case prog ! pc st of
    ("snd":xs:_) -> (st1, Just $ getVal xs)
    ("set":xss)  -> calc xss (flip const)
    ("add":xss)  -> calc xss (+)
    ("mul":xss)  -> calc xss (*)
    ("mod":xss)  -> calc xss mod
    ("rcv":[r]:_) -> 
      case q st of
        Q.Empty -> (st {status = Wait}, Nothing)
        (v Q.:<| vs) -> (st1 {status = Ready, q = vs, regs = regs st // [(r, v)]}, Nothing)
    ("jgz":xs:ys:_) -> (st {pc = pc st + if getVal xs > 0 then getVal ys else 1}, Nothing)
  where
    st1 = st {pc = succ $ pc st}
    getVal :: String -> Int
    getVal [c] | 'a' <= c, c <= 'z' = regs st ! c
    getVal xs = read xs
    calc :: [String] -> (Int -> Int -> Int) -> (State, Maybe Int)
    calc ([r]:ys:_) f = (st1 {regs = regs st // [(r, f (regs st ! r) (getVal ys))]}, Nothing)

run :: Prog -> [Int]
run prog = runloop state0 state1
  where
    runloop st0 st1 =
      case (status st0a, status st1a) of
        (_,Ready) -> output $ runloop st1a st0a
        (Ready,_) -> output $ runloop st0a st1a
        _ -> []
      where
        (st0a, mmsg) = step prog st0 -- 0を動かす
        st1a = maybe st1 (enQ st1) mmsg -- 0が送信していたら1に受けさせるので、Waitなら解消される
        output = if idee st0 == 1 then maybe id (:) mmsg else id

sample = compile "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d\n"

main2 = do
  co <- readFile "input.txt"
  let out1 = run $ compile co
  print $ length out1

{-
*Main> run sample
[1,2,1]
*Main> main2
838708
また力押ししてしまった。こんなプログラムを読んで最適化したくないよ。
your answer is too high.
あれー。

*Main> main2
8001
WaitにしているのにPC++して進めてしまっていた。てへ。
-}
