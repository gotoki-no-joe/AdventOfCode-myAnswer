import qualified Data.Set as S
import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import Data.List.Split
import Numeric
import Data.List

part1 fn body =
  do
    ls <- lines <$> readFile fn
    print $ body ls

solve1 xy0 ls = S.size xysZ
  where
    xys = scanl step (0,0) [dir | l <- ls, let [dir]:ns:_ = words l, _ <- [1 .. read ns]]

    step (x,y) 'U' = (x, pred y)
    step (x,y) 'D' = (x, succ y)
    step (x,y) 'R' = (succ x, y)
    step (x,y) 'L' = (pred x, y)

    xysZ = loop (S.fromList xys) [xy0]
    loop s [] = s
    loop s (xy:xys)
      | S.member xy s = loop s xys
      | otherwise = loop (S.insert xy s) ((pred x, y):(succ x, y):(x, pred y):(x, succ y):xys)
      where
        (x,y) = xy

{-
ghci> part1 "sample.txt" (solve1 (1,1))
62
ghci> part1 "input.txt" (solve1 (1,1))
28911

パート2どうすんだ。
ピクセルだと区別つかないけど、縦横がU,D,L,Rでわかるので、edge to edge で考える？クロスしてたらアウトだが、そもそもそれだと中と外が不明瞭なのでないだろう。
横線が重なるとやっかいだけど、そういうのもないと仮定しよう。
向きが同じになっても大丈夫だから、縦線はedgeとして、横線は両端だけをedgeとして登録して、
水平線に切って、間を数える感じ。
まずそれでpart1を数えて、正しくできるか検証する。
-}

solve2 dks = ans + sum (map snd dks)
  where
    loop xy [] = []
    loop (x,y) (('U',k):dks)
      | next dks == 'L' = [(x, y1) | y1 <- [y -      k .. pred y]] ++ loop (x, y - k) dks
      | otherwise       = [(x, y1) | y1 <- [y - pred k .. pred y]] ++ loop (x, y - k) dks
    loop (x,y) (('D',k):dks)
      | next dks == 'R' = [(x, y1) | y1 <- [succ y .. y +      k]] ++ loop (x, y + k) dks
      | otherwise       = [(x, y1) | y1 <- [succ y .. y + pred k]] ++ loop (x, y + k) dks
    loop (x,y) (('R',k):dks)
      | next dks == 'U' = (x + k, y) : loop (x + k, y) dks
      | otherwise       =              loop (x + k, y) dks
    loop (x,y) (('L',k):dks)
      | next dks == 'D' = (x - k, y) : loop (x - k, y) dks
      | otherwise       =              loop (x - k, y) dks

    next [] = fst $ head dks
    next ((dir,_):_) = dir

    im = IM.fromListWith (++) $ map (\(x, y) -> (y, [x])) $ loop (0,0) dks

    ans = sum $ map count $ IM.elems im
    count is = sum $ map (\(a:b:_) -> pred b - a) $ chunksOf 2 $ sort is

parse1 l = (dir, read ns) 
  where
    [dir]:ns:_ = words l

{-
やっぱりダメ。水平線が、領域を作ったり作らなかったりするので。
ではどうしたらいいんだろう？
縦の線について、下向きの左、上向きの右にマークを置いて、内側だけを数えて、線上の点の個数と合わせる？
グルグル回しても、クロスがない限り、それは入れ替わらない。
線の端もマークしないよう注意。
U->Rは終端なし、U->Lなら終端付ける。
D->Lは終端なし、D->Rなら終端付ける。
R->Dは終端なし、R->Uなら次の始点つける。
L->Uは終端なし、L->Dなら次の始点つける。

幅1のときに重なってしまわないように、線上にマークして、数えるときに間だけを数える。

ghci> part1 "sample.txt" (solve2 . map parse1)
62
ghci> part1 "input.txt" (solve2 . map parse1)
28911
-}

parse2 l = ("RDLU" !! mod x 4, div x 16)
  where
    x = fst $ head $ readHex $ drop 2 $ words l !! 2

{-
ghci> part1 "sample.txt" (solve2 . map parse2)
952408144115
うん、合ってる。
-}

main = do
  part1 "sample.txt" (solve2 . map parse1)
  part1 "input.txt" (solve2 . map parse1)
  part1 "sample.txt" (solve2 . map parse2)
  part1 "input.txt" (solve2 . map parse2)

{-
> ./ver1
62
28911
952408144115
77366737561114

IntSetの無駄をはぶいてData.List.sortで済ませて、コンパイルして、ようやく達成。
縦に同じ状況が続く区間を圧縮する、みたいなテクニックが必要だったろうか？ようやらんかな。
-}
