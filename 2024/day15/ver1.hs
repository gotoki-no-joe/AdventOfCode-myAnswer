--import Data.List
import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Data.List
import Data.List.Split

import qualified Data.IntSet as IS

import Debug.Trace

runner i f = do
  (ls1,_:ls2) <- break null . lines <$> readFile i
  let h = length ls1
      w = length $ head ls1
      arr = listArray ((0,0),(pred h,pred w)) $ concat ls1
  print $ f arr (concat ls2)

test0 = runner "samp0.txt" part1
test1 = runner "samp1.txt" part1
main1 = runner "input.txt" part1

part1 :: UArray (Int,Int) Char -> String -> Int
part1 arr cmds = runST $
  do
    fld <- thaw arr
    foldM_ (step fld) pos0 cmds
    getResult fld
--    intercalate "\n" . chunksOf w <$> getElems fld
  where
--    w = succ $ snd $ snd $ bounds arr
    pos0 = head [pos0 | (pos0, '@') <- assocs arr]
    getResult :: STUArray s (Int, Int) Char -> ST s Int
    getResult fld = do
      res <- getAssocs fld
      return $ sum [i * 100 + j | ((i,j),'O') <- res]
    step :: STUArray s (Int, Int) Char
         -> (Int, Int) -> Char -> ST s (Int, Int)
    step fld pos cmd =
      do
        mend <- findEnd pos1
        case mend of
          Nothing -> return pos
          Just end -> do
            writeArray fld end 'O'
            writeArray fld pos '.'
            writeArray fld pos1 '@'
            return pos1
      where
        vec = dvec cmd
        pos1 = add pos vec
        findEnd op = do
          x <- readArray fld op
          case x of
            '.' -> return $ Just op
            'O' -> findEnd (add op vec)
            _   -> return Nothing

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c, b+d)

dvec :: Char -> (Int, Int)
dvec '^' = (-1,0)
dvec 'v' = ( 1,0)
dvec '>' = (0, 1)
dvec '<' = (0,-1)

{-
倉庫番ですな。
ロボットの位置を取り出す

ロボの前方が、0個以上の 'O' と、次が '.' で終わっているとき、
'.' の位置に'O' を、ロボの前方に '@' を、ロボの位置に '.' を書き込めばアニメーション完成

色々バグってて泣きそうだった。

そしてパート2、泣きそう。
横に押すときは1列ずつなので、まだまし。といっても、[と]は区別しないといけなので面倒ではある。
縦に押すとき、押される主体を全員調べ上げて、それらが全員ひっかからずに押せることを確認して、押す、という動きが必要。
なにそれぇ。

行yから上に押すとき、押し圧力がかかる点は {y} と IntSet (またはリスト) で表せる。
次の行で圧力ポイントを見て、どこかに '#' があったら動かせない、で終了。
'.' は押すことができて、次の圧力ポイントにはならない。
'[' または ']' は押す対象なので、もう片割れの荷物も圧力ポイントになる。
圧力ポイントが空になる行まで逃げ切ったら、押すことができる。

そうしたら、今度は遠い方からロボの方に、圧力ポイントの列の内容を上の行に移動させる。
最終的にロボまで上に動かして完成。
箱がぴったりかさなっていると、ひとつの箱を二度押したりしそうなので、IntSetで重複を消す方が楽かもだ。

-}

runner2 i f = do
  (ls1,_:ls2) <- break null . lines <$> readFile i
  let h = length ls1
      w = length $ head ls1
      arr = listArray ((0,0),(pred h, w + pred w)) $
            concatMap doubler $ concat ls1 :: UArray (Int,Int) Char
--  mapM_ putStrLn $ chunksOf (w+w) $ elems arr
  print $ f arr (concat ls2)

doubler :: Char -> String
doubler '@' = "@."
doubler 'O' = "[]"
doubler c = [c,c]

test2 = runner2 "samp1.txt" part2
main2 = runner2 "input.txt" part2

part2 :: UArray (Int,Int) Char -> String -> Int
part2 arr cmds = runST $
  do
    fld <- thaw arr
    foldM_ (step fld) pos0 cmds
    getResult fld
  where
    pos0 = head [pos0 | (pos0, '@') <- assocs arr]
    getResult :: STUArray s (Int, Int) Char -> ST s Int
    getResult fld = do
      res <- getAssocs fld
      return $ sum [i * 100 + j | ((i,j),'[') <- res]
    step :: STUArray s (Int, Int) Char
         -> (Int, Int) -> Char -> ST s (Int, Int)
    step fld pos cmd
      | elem cmd "<>" =  -- 右または左への移動
      do
--        traceShow cmd $ return ()
        b <- loop fld pos
        return $ if b then add pos vec else pos
      where
        vec = dvec cmd
--        loop :: (Int,Int) -> ST s Bool
        loop fld pos = do
          x <- readArray fld pos
          case x of
            '#' -> return False
            '.' -> return True
            _ -> do
              let pos1 = add pos vec
              b <- loop fld pos1
              when b $ do
                c <- readArray fld pos
                writeArray fld pos1 c
                writeArray fld pos '.'
              return b
    step fld pos@(i,j) cmd
      | elem cmd "^v" =  -- 上または下への移動
      do
--        traceShow cmd $ return ()
        b <- loop fld i (IS.singleton j)
        return $ if b then add pos vec else pos
      where
        vec = dvec cmd
--        loop :: (Int,Int) -> ST s Bool
        loop fld i jS = do
          xjs <- forM (IS.elems jS) (\j -> do -- jの指す位置の文字を、位置を添えて全て読み出す
            c <- readArray fld (i,j)
            return (c,j))
          let xs = map fst xjs
          case () of
            _ | elem '#' xs -> return False -- '#' があったらアウト
              | all ('.' ==) xs -> return True -- 全部 '.' ならそこで終わり
              | otherwise -> do
                let jS1 = IS.fromList $ concatMap udfunc xjs -- 今回動きそうな要素のj座標=次の圧力ポイントを構築
                    i1 = i + fst vec
                b <- loop fld i1 jS1
                when b $ do
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
