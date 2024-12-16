倉庫番は日本発のゲームだから、世界でも "Sokoban" なんだよね。
雑誌に掲載されていた他機種用のBASICプログラム移植して遊んだ在りし日の記憶。

# 入力

空行で切り、前半の地図は `UArray (Int,Int) Char` に、
（Haskellの配列の都合で、座標系はX,Yではなく縦、横の順になる）
後半のコマンドは一列に繋げて渡せばよいだろう。

GPS計算がしやすいように、座標の範囲は1始まりではなく0始まりにしておく。

自分は ST モナドの方が好きだが、今回はフィールドの様子を途中で表示したくなりそうなので、
IO モナドで本体を構成する。

```haskell
import Data.Array.Unboxed

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

part1 :: UArray (Int,Int) Char -> String -> IO Int
part1 arr cmds = ...
```

# パート1

コマンドの文字から、移動のベクトルを取り出す写像と、座標にそれを足し込む計算は退屈なコード。

```haskell
type POS = (Int,Int)

add :: POS -> POS -> POS
add (a,b) (c,d) = (a+c, b+d)

dvec :: Char -> POS
dvec '^' = (-1,0)
dvec 'v' = ( 1,0)
dvec '>' = (0, 1)
dvec '<' = (0,-1)
```

フィールドの様子はずっと維持する必要があるので、
実行速度を考えて `IOUArray (Int,Int) Char` で表すことにする。

先に、フィールドを初期設定したり、フィールドを確認のために画面出力したり、
返すべきスコアを計算するところを書いてしまおう。

```haskell
import Control.Monad
import Data.Array.IO
import Data.List
import Data.List.Split

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
    getResult fld

-- フィールドの表示
showField :: Field -> IO ()
showField fld = do
  (_,(_,w)) <- getBounds fld
  cs <- getElems fld
  putStrLn . intercalate "\n" . chunksOf (succ w) $ cs

-- 結果の取り出し
getResult :: Field -> IO Int
getResult fld = do
  res <- getAssocs fld
  return $ sum [i * 100 + j | ((i,j),'O') <- res]

-- 可能なら移動を実行し、移動後のロボの位置を返す
doMove :: Field -> POS -> Char -> IO POS
doMove fld p0 cmd = ...
```

`doMove` でひとつの移動コマンドを実行する。
ロボットの現在位置 $p_0$ と次の移動のベクトルを使って、
ロボットの一歩先 $p_1$ から、0個以上の荷物の連続があって、最後に床で終わってるとき、
その床の座標を $p_2$ として、

1. $p_2$ に荷物を書く
2. $p_1$ にロボットを書く
3. $p_0$ に床を書く

とすれば、荷物が実は0個でも、問題無く全体が移動できる。
前方を調査している途中で壁に遭遇したときは失敗で、その移動は行わない。

```haskell
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
```

# パート2

やられた。`runner` の作る配列は役に立たない。
横に長いフィールドの文字配列を作るコードは作り直し。

```haskell
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
doubler  c  = [c,c]

test2 = runner2 "samp1.txt" part2
main2 = runner2 "input.txt" part2

part2 :: UArray POS Char -> String -> IO Int
part2 = ...
```

`part1` のコードのうち、フィールドを作る `thaw` フィールドを表示 `showField` はそのまま使える。
結果の取り出し `getResult` は、`O` でなく `[` を数えるマイナーな変更だけで使える。

```haskell
-- 結果の取り出し
getResult :: Char -> Field -> IO Int
getResult tgt fld = do
  res <- getAssocs fld
  return $ sum [i * 100 + j | ((i,j),c) <- res, c == tgt]
```

パターンマッチでのふるい落としができなくなったのでガードを使う。
`part1` もこれで動くように直しておいて。

## 左右移動

移動の計算が大きく変わる。

簡単そうな左右移動から。それでも、`O` の列の先頭と末尾だけ書き換えて誤魔化すことができないので、
先頭の `.` を探し、ロボまで全体を1文字ずつちゃんと移動させる必要がある。

移動のための `fld` への書き込みは、（mutable arrayなので）奥の方から先にしないと、上書きでおかしくなる。
という諸々を考えて、ループの仕事を「先頭の空白を探す」ではなく、移動まで実行させる。

- 先頭の空白まで再帰呼び出しで奥へ進む。
- 壁に当たったらそれ以上何もせず `False` を返す。
- 空白を見つけたなら、`True` を返す。
- 再帰呼び出しから返ってきた値が `False` なら、自分も何もしない。
- `True` だったら、自分の位置の内容を次の位置に書き込み、自分の位置には `.` を書き込む

を、ロボットの位置から始めるようにする。
最後の手順は、どうせもう一つ手前の移動で上書きされるので無駄に見えるが、
バカ正直にこうしておく。

```haskell
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
            readArray fld pos >>= writeArray fld pos1
            writeArray fld pos '.'
          return b
```

## 上下移動

この問題のハイライト。

横移動では、ある列について前に移動しようとするのは一点だけだった。
縦移動では、押す力のかかっている全ての点が、一斉に次の行に移動しようとする。
それらのうち、誰か一人でも壁に阻まれるとき、移動全体が失敗する。

ということで、横移動の `loop` では `pos` の縦座標として押したい位置を一点だけ送っていたものを、
「前に進もうとしている荷物のある横座標の集合」を送る形に拡張すればできる。

```haskell
import qualified Data.IntSet as IS

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
```

部品を組み立てたら完成。

```haskell
part2 :: UArray POS Char -> String -> IO Int
part2 arr cmds =
  do
    let pos0 = head [pos0 | (pos0, '@') <- assocs arr]
    fld <- thaw arr :: IO Field
    foldM_ (doMove2 fld) pos0 cmds
    showField fld
    getResult '[' fld
```
