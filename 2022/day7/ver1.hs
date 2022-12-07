-- 2022-12-7

{-
これは面倒くさいな。
cdの履歴で現在の位置を把握しながら、
その中のファイルのサイズを記録していく。

個々のファイルの大きさは必要ないので、
ディレクトリと、その中のファイルのサイズ
の対を作って、マップに足しこむか。
しかしそれだと、階層を把握できない。
やはり木構造をちゃんと作るべきか。
重複することはあるのかな。

木構造にしても、ジャンプがあるのがねぇ。
現在位置を、文字列でなく、ディレクトリ名のリストにするか。
ファイル名までそれで取り込むか。
-}

import qualified Data.Map as M
import Data.List
import Data.Maybe

parseFile :: [String] -> M.Map [String] Int
parseFile = M.fromList . catMaybes . snd . mapAccumL step []
  where
    step :: [String] -> String -> ([String], Maybe ([String], Int))
    step dir l =
      case words l of
        ["$", "cd", ".."] -> (tail dir, Nothing)
        ["$", "cd", "/"] -> ([], Nothing)
        ["$", "cd", d] -> (d:dir, Nothing)
        ["$", "ls"] -> (dir, Nothing)
        ["dir", _] -> (dir, Nothing)
        [n, fn] -> (dir, Just (fn:dir, read n))
        _ -> error $ "unexpected" ++ l

body1 fn = readFile fn >>= print . sum . filter (100000 >=) . M.elems . accumlate . parseFile . lines

accumlate :: M.Map [String] Int -> M.Map [String] Int
accumlate m = M.fromListWith (+)
  [ (p, sz)
  | (path, sz) <- M.assocs m
  , p <- tails (tail path)
  ]

main1 = body1 "input.txt"
test1 = body1 "test.txt"

{-
70000000 - root の使用量 + 解放容量 >= 30000000
解放容量 >= rootの使用量 - 40000000
この条件を満たす最小のフォルダの容量を言え、といっている。
-}

body2 fn = do
  m <- accumlate . parseFile . lines <$> readFile fn
  let limit = m M.! [] - 40000000
  print . minimum . filter (limit <=) . M.elems $m

main2 = body2 "input.txt"
test2 = body2 "test.txt"
