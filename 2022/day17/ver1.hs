{-
こういう二次元のどうたらこうたら、命令型言語指向でスキじゃないのよさ。

無限に伸びるステージを表現して、
落ちてくる岩の形を作って、
ぶつからずに移動する振る舞いを作る
という感じか。

岩の個数2022ということだと、風のパターンが大きいのは大して障害にならないよな。
まぁいい。

幅7なので、8ビットデータの列で表現するのが一番コンパクトな印象はある。
いくらでも伸ばせるのをどう表すかだが、
とりあえず、全部積みあがったとして13/5 * 2022 = 5257.2 なので、5300あれば足りるのでそうしよう。

ビット配列でやるなら、岩の表現もビットにした方がいいな。
と思ったけど、はみ出す、を検出するのが面倒だったわ。
-}

import Control.Monad
import Data.Array.IO
import Data.Word
import Data.Bits

{-
左下からの相対位置で、岩の形を表す
-}

type Rock = [(Int,Int)]
rocks :: [Rock]
rocks =
  [ [(0,0),(1,0),(2,0),(3,0)]
  , [(1,0),(0,1),(1,1),(2,1),(1,2)]
  , [(0,0),(1,0),(2,0),(2,1),(2,2)]
  , [(0,0),(0,1),(0,2),(0,3)]
  , [(0,0),(0,1),(1,0),(1,1)]
  ]

-- 座標の足し算
add (x,y) (z,w) = (x+z,y+w)

{-
IOArray Int Word8 なステージに対して、点列が指す位置がすべてフリーかを調べる
-}

type Stage = IOArray Int Word8

-- 指定位置の全てがbit 0ならTrue
allFree :: Stage -> [(Int,Int)] -> IO Bool
allFree stage xys =
  and <$> forM xys (\(x,y) ->
    if x < 0 || x > 6 then return False else
      not . flip testBit x <$> readArray stage y
    )

-- 岩のある最高の高さを返す
topOfRocks :: Stage -> IO Int
topOfRocks stage = do
  bnds <- getBounds stage
  pred . length . takeWhile (0 /=) <$> forM (range bnds) (readArray stage)

-- 岩を風で流し、下にずらす。
-- 風で流れるとき動けないのは無視、下にずらそうとして動けないとき止まる。
-- 止まったとき、Stageに自分を書き込み、Nothingを返す
-- 動くときはStageはそのままで、移動後の(x,y) をRight (x,y)で返す

step :: Stage -> Rock -> Char -> (Int,Int) -> IO (Maybe (Int,Int))
step stage rock wind (x,y) =
  do
    chk1 <- allFree stage rxys1
    let (rxys4, xx) = if chk1 then (rxys3, x1) else (rxys2, x)
    chk2 <- allFree stage rxys4
--    print (chk1, chk2)
    if chk2 then return (Just (xx, pred y)) else do
      let rxys = if chk1 then rxys1 else rxys0
      forM_ rxys (\(x,y) -> writeArray stage y . flip setBit x =<< readArray stage y)
      return Nothing
  where
    x1 = (if wind == '>' then succ else pred) x
    rxys0 = map (add (x,y)) rock -- 最初の岩
    rxys1 = map (add (x1, y)) rock -- 流された岩
    rxys2 = map (add (x,  pred y)) rock -- 流されずに落ちる岩
    rxys3 = map (add (x1, pred y)) rock -- 流されて落ちた岩

-- 0になるところまで、ステージを描画する
displayStage :: Stage -> IO ()
displayStage stage =
  do
    mapM_ putStrLn . take 20 =<< loop 0 []
  where
    loop :: Int -> [String] -> IO [String]
    loop y ls = do
      b <- readArray stage y
      if b == 0 then return ls else do
        let cs = map (\x -> if testBit b x then '#' else '.') [0..6]
        loop (succ y) (('|' : cs ++ "| " ++ show y):ls)

test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

-- 岩と風列と最初の高さを与えると、岩が落ちるまで落として、消費しなかった風列を返す
fallOne :: Stage -> Rock -> String -> Int -> IO String
fallOne stage rock wind y0 =
  loop wind (2,y0)
  where
    loop (w:ws) xy = do
      er <- step stage rock w xy
      case er of
        Nothing -> return ws
        Just xy1 -> loop ws xy1
    loop [] _ = error "never"

test1 = do
  stage <- newArray (0,5300) 0
  writeArray stage 0 255 -- 床
  ws <- fallOne stage (head rocks) test 3
  y1 <- topOfRocks stage
  putStrLn "2"
  fallOne stage (rocks !! 1) ws (y1+4)
  displayStage stage

phase1 nums winds = do
  stage <- newArray (0,5300) 0
  writeArray stage 0 255 -- 床
  (h,_) <- foldM (\(y0,ws) rock -> do
    ws1 <- fallOne stage rock ws (y0 + 4)
    y1 <- topOfRocks stage
    return (y1, ws1)
    ) (0, cycle winds) $ take nums $ cycle rocks
  displayStage stage
  print h

test2 = phase1 5 test
test3 = phase1 2022 test

main1 = do
  wind <- head . lines <$> readFile "input.txt"
  phase1 2022 wind

{-
part 2
周期性を発見するのだと思うけども、どうなってるんだろう。
rocksが5の倍数で落ちて行って、ちょうどwindsが使い切られた瞬間の
てっぺんが平ら、みたいなことになっていればうれしいのだけどねぇ。

とりあえず、ひとつ落とすごとに、windをいくつ消費したかわかるようにして(たかだか3)
合計が元の長さの倍数になったところで、rockも5の倍数になったところで、てっぺんを見る、
を、してみよう。
-}

-- 岩と風列と最初の高さを与えると、岩が落ちるまで落として、
-- 使った風の個数を返す
fallOne1 :: Stage -> Rock -> String -> Int -> IO Int
fallOne1 stage rock wind y0 =
  loop 0 wind (2,y0)
  where
    loop cnt (w:ws) xy = do
      er <- step stage rock w xy
      case er of
        Nothing -> return cnt
        Just xy1 -> loop (succ cnt) ws xy1
    loop _ [] _ = error "never"

phase2 nums winds =
  do
  stage <- newArray (0,5300) 0
  writeArray stage 0 255 -- 床
  (h,_) <- foldM (\(y0,ws) rock -> do
    ws1 <- fallOne stage rock ws (y0 + 4)
    y1 <- topOfRocks stage
    return (y1, ws1)
    ) (0, cycle winds) $ take nums $ cycle rocks
  displayStage stage
  print h
