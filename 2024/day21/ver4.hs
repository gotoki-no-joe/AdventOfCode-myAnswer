import qualified Data.Map as M
import Data.List.Split
import Data.List
import Data.Function

import qualified Data.Set as S

import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe

type Pad = M.Map (Int,Int) Char

numericPad, directionalPad :: Pad
numericPad = makeKeypad "789456123*0A"
directionalPad = makeKeypad "*^A<v>"

makeKeypad :: String -> Pad
makeKeypad str = M.fromList
  [ ((i,j),c)
  | (i, cs) <- zip [0 ..] $ chunksOf 3 str
  , (j, c ) <- zip [0 ..] cs, c /= '*' ]

type Paths = M.Map (Char,Char) [String]

numericPaths, directionalPaths :: Paths
numericPaths     = allPaths numericPad
directionalPaths = allPaths directionalPad

allPaths :: Pad -> Paths
allPaths pad =
    M.mapKeys (\(ij, pq) -> (pad M.! ij, pad M.! pq)) $
    M.map (map (reverse . ('A' :))) m
  where
    m = M.fromDistinctAscList
      [((s,e), findLowestCostPaths s e) -- 最短操作系列を（逆順で）求める
      | s <- M.keys pad                 -- sとeのキーの座標に関して全ての組み合わせで
      , e <- M.keys pad]
    findLowestCostPaths s e             -- 出発点s終点e
      | s == e = [""]                   -- s = e なら操作はε
      | otherwise = shortests           -- 最短のもの全て
        [ d : p                         -- nからeへの1操作を追加したもの
        | (n,d) <- closer s e           -- eよりsに一歩近い隣nの
        , M.member n pad                -- ただしnは実在するもの限定で
        , p <- m M.! (s,n)]             -- sからnへの最短系列に

-- (p,q) より (i,j) に近い (p,q) の隣接点とそこから(p,q)への移動コマンド
closer :: (Int,Int) -> (Int,Int) -> [((Int,Int),Char)]
closer (i,j) (p,q) =
  [((pred p, q), 'v') | i < p] ++ [((succ p, q), '^') | p < i] ++
  [((p, pred q), '>') | j < q] ++ [((p, succ q), '<') | q < j]

shortests [] = []
shortests xs = [x | (l,x) <- zip ls xs, l == lmin]
  where
    ls = map length xs
    lmin = minimum ls

-- part1

makePath :: String -> String
makePath code = findCode3 code
  where
    findCode findCodePrev path code = concat
      [ shortest (map findCodePrev p)
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
    findCodeHead code = code -- 自分は必要なコードをそのまま打ち込む
    findCode1 = findCode findCodeHead directionalPaths
    findCode2 = findCode findCode1    directionalPaths
    findCode3 = findCode findCode2    numericPaths

shortest xs = minimumBy (compare `on` length) xs

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = sum [ length (makePath l) * read (init l) | l <- ls]

calcPathLen :: Int -> String -> Int
calcPathLen sysDepth code = findCodeLen sysDepth code
  where
    findCodeLen 0     code = length code -- 自分
    findCodeLen depth code = sum
      [ minimum (map (findCodeLen (pred depth)) p)
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
      where
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1a d ls = sum [calcPathLen d l * read (init l) | l <- ls]

memoize :: Ord t => ((t -> s) -> t -> ([t], s)) -> t -> s
memoize fya x = m M.! x
  where
    m = loop M.empty (S.singleton x)
    loop old new
      | S.null new = old
      | otherwise  = loop old1 new1
      where
        (kvs, jss) = unzip [((k,v),js) | k <- S.elems new, let (js, v) = fya (m M.!) k]
        old1 = M.union old $ M.fromList kvs
        new1 = S.fromList $ concatMap (filter (flip M.notMember old1)) jss

calcPathLenM :: Int -> String -> Int
calcPathLenM sysDepth code = memoize findCodeLen (sysDepth, code)
  where
    findCodeLen _     (0    , code) = ([], length code)
    findCodeLen recur (depth, code) = (concat recargss, result)
      where
        ps = map (path M.!) $ zip ('A':code) code
        recargss = [[(pred depth, e) | e <- p] | p <- ps]
        result = sum [minimum (map recur recargs) | recargs <- recargss]
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1b d ls = sum [calcPathLenM d l * read (init l) | l <- ls]

main2 = runner "input.txt" (part1b 26)

memoizeU :: Ord d => ((d -> a) -> (d -> a)) -> (d -> a)
memoizeU mf = f
  where
    memo = unsafePerformIO $ newIORef M.empty
    f x = unsafePerformIO $ do
      m <- readIORef memo
      case M.lookup x m of
        Just a -> return a
        Nothing -> do
          let a = mf f x
          modifyIORef' memo (M.insert x a)
          return a

calcPathLenU :: Int -> String -> Int
calcPathLenU sysDepth code = memoizeU findCodeLen (sysDepth, code)
  where
    findCodeLen _     (0    , code) = length code
    findCodeLen recur (depth, code) = sum
      [ minimum [recur (pred depth, e) | e <- p]
      | ab <- zip ('A':code) code
      , let p = path M.! ab ]
      where
        path | depth == sysDepth = numericPaths
             | otherwise         = directionalPaths

part1c d ls = sum [calcPathLenU d l * read (init l) | l <- ls]

main2U = runner "input.txt" (part1c 26)
