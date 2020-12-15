import Data.Bits
import Data.Char
import qualified Data.Map as M

{-
これはビット演算をすることと、ファイルを読み込むことの二つをしないといけない。

マスクは、
X データそのまま
0 データにnotをandする
1 データにorする
-}

sample = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"

type Mask = (Int,Int) -- or Or and And

data Instr = Mask Mask | Write Int Int deriving Show

parse1 str
  | str !! 1 == 'a' = parseMask str
  | True            = parseWrite str

parseMask str = Mask $ foldl step (0,0) (drop 7 str)
  where
    step (o,a) c = case c of
      'X' -> (o1, a1)
      '0' -> (o1, a1 `clearBit` 0)
      '1' -> (o1 `setBit` 0, a1)
      where
        o1 = o `shiftL` 1
        a1 = (a `shiftL` 1) `setBit` 0

parseWrite str = Write (read s1) (read s3)
  where
    (s1,s2) = span isDigit (drop 4 str)
    s3 = drop 4 s2

exec :: [Instr] -> M.Map Int Int
exec is = snd $ foldl step ((0,2^36-1), M.empty) is -- mask may be undefined
  where
    step (mask,mem) (Mask mask1) = (mask1,mem)
    step (mask@(o,a),mem) (Write ad da) = (mask, M.insert ad ((da .|. o) .&. a) mem)

comp1 str = sum $ M.elems $ exec $ map parse1 $ lines str

test1 = comp1 sample

ans1 = readFile "input.txt" >>= print . comp1

{-
*Main> test1
165
*Main> ans1
7611244640053
-}
