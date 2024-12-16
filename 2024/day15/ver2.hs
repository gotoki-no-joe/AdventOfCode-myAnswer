import Data.Array.Unboxed

import Control.Monad
import Data.Array.IO
import Data.List
import Data.List.Split

import qualified Data.IntSet as IS

runner i f = do
  (ls1,_:ls2) <- break null . lines <$> readFile i
  let h = length ls1
      w = length $ head ls1
      arr = listArray ((0,0),(pred h,pred w)) $ concat ls1
  res <- f arr (concat ls2)
  print res

test0 = runner "samp0.txt" part1
test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

type POS = (Int,Int)
type Field = IOUArray POS Char

part1 :: UArray (Int,Int) Char -> String -> IO Int
part1 arr cmds =
  do
-- ロボの初期位置
    let pos0 = head [pos0 | (pos0, '@') <- assocs arr]
-- フィールドを作る
    fld <- thaw arr :: IO Field
-- 移動の計算
    foldM_ (doMove fld) pos0 cmds
-- フィールドを表示
    showField fld
-- 結果の取り出し
    getResult 'O' fld

-- 可能なら移動を実行し、移動後のロボの位置を返す
doMove :: Field -> POS -> Char -> IO POS
doMove fld p0 cmd =
  do
    res <- findEnd p1
    case res of
      Nothing -> return p0
      Just p2 -> do
        writeArray fld p2 'O'
        writeArray fld p1 '@'
        writeArray fld p0 '.'
        return p1
  where
    vec = dvec cmd
    p1 = add p0 vec
    findEnd p = do -- O*. と並んでいる . の位置を探す
      x <- readArray fld p
      case x of
        '.' -> return $ Just p
        'O' -> findEnd (add p vec)
        '#' -> return Nothing

add :: POS -> POS -> POS
add (a,b) (c,d) = (a+c, b+d)

dvec :: Char -> POS
dvec '^' = (-1,0)
dvec 'v' = ( 1,0)
dvec '>' = (0, 1)
dvec '<' = (0,-1)

-- フィールドの表示
showField :: Field -> IO ()
showField fld = do
  (_,(_,w)) <- getBounds fld
  cs <- getElems fld
  putStrLn . intercalate "\n" . chunksOf (succ w) $ cs

-- 結果の取り出し
getResult :: Char -> Field -> IO Int
getResult tgt fld = do
  res <- getAssocs fld
  return $ sum [i * 100 + j | ((i,j),c) <- res, c == tgt]

-- part2

runner2 i f = do
  (ls1,_:ls2) <- break null . lines <$> readFile i
  let h = length ls1
      w = length $ head ls1
      arr = listArray ((0,0),(pred h, w + pred w)) $
            concatMap doubler $ concat ls1
  res <- f arr (concat ls2)
  print res

doubler :: Char -> String
doubler '@' = "@."
doubler 'O' = "[]"
doubler c = [c,c]

test2 = runner2 "samp1.txt" part2
main2 = runner2 "input.txt" part2

-- 可能なら移動を実行し、移動後のロボの位置を返す
doMove2 :: Field -> POS -> Char -> IO POS
doMove2 fld p0 cmd
  | elem cmd "<>" =  -- 右または左への移動
  do
    b <- loop fld p0
    return $ if b then add p0 vec else p0
  where
    vec = dvec cmd
    loop fld pos = do
      x <- readArray fld pos
      case x of
        '#' -> return False  -- 壁に当たった。失敗。
        '.' -> return True   -- 空き床を見つけた。成功。
        _ -> do -- `[` か `]`
          let pos1 = add pos vec
          b <- loop fld pos1 -- さらに前を調べる
          when b $ do        -- 成功のときは、自分を前に動かす
            c <- readArray fld pos
            writeArray fld pos1 c
            writeArray fld pos '.'
          return b

doMove2 fld p0@(i,j) cmd
  | elem cmd "^v" =  -- 上または下への移動
  do
    b <- loop fld i (IS.singleton j)
    return $ if b then add p0 vec else p0
  where
    vec = dvec cmd
    loop fld i jS = do
      xjs <- forM (IS.elems jS) (\j -> do -- jの指す位置の文字を、位置を添えて全て読み出す
        c <- readArray fld (i,j)
        return (c,j))
      let xs = map fst xjs
      case () of -- multiway if
        _ | elem '#' xs     -> return False -- '#' があったらアウト
          | all ('.' ==) xs -> return True -- 全部 '.' なら成功。
          | otherwise       -> do
            let jS1 = IS.fromList $ concatMap udfunc xjs -- 今回動きそうな要素のj座標=次の圧力ポイントを構築
                i1 = i + fst vec
            b <- loop fld i1 jS1 -- さらに前を調べる
            when b $ do          -- 成功のときは、横並び全員を前に動かす
              forM_ (IS.elems jS1) (\j -> do
                readArray fld (i, j) >>= writeArray fld (i1, j)
                writeArray fld (i, j) '.'
                )
            return b

udfunc :: (Char, Int) -> [Int]
udfunc ('@',j) = [j]
udfunc ('[',j) = [j, succ j]
udfunc (']',j) = [pred j, j]
udfunc ('.',_) = []

part2 :: UArray POS Char -> String -> IO Int
part2 arr cmds =
  do
-- ロボの初期位置
    let pos0 = head [pos0 | (pos0, '@') <- assocs arr]
-- フィールドを作る
    fld <- thaw arr :: IO Field
-- 移動の計算
    foldM_ (doMove2 fld) pos0 cmds
-- フィールドを表示
    showField fld
-- 結果の取り出し
    getResult '[' fld
