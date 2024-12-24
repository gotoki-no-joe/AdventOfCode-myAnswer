import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List

parse1 :: String -> (String, Bool)
parse1 l = (take 3 l, last l == '1')

parse2 :: String -> (String, String, String, String)
parse2 l = (a,b,c,d)
  where
    a:b:c:_:d:_ = words l

runner i f = do
  (ls1, _:ls2) <- break null . lines <$> readFile i
  let p1 = map parse1 ls1
  let p2 = map parse2 ls2
  print $ f p1 p2

test1 = runner "samp1.txt" part1
test2 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

part1 p1 p2 =
    sum $
    zipWith (\i b -> if b then i else 0) (iterate (2 *) 1) $
    map (m M.!) $ takeWhile (flip M.member m)
    ['z' : tail (show i) | i <- [100 ..]]
  where
    m = M.fromList $ p1 ++ [(d, op b (m M.! a) (m M.! c)) | (a,b,c,d) <- p2]
    op "OR" = (||)
    op "AND" = (&&)
    op "XOR" = (/=)

main2 = runner "input.txt" part2

part2 _ p2 = checkloop 1 c00
  where
--    g = M.fromListWith (++) [((min a c, max a c), [(b, d)]) | (a,b,c,d) <- p2]
    graph a b = M.findWithDefault [] (min a b, max a b) g

    check k cin = sub x y cin <|> sub y cin x <|> sub cin x y
      where
        x = 'x' : tail (show $ k + 100)
        y = 'y' : tail x
        z = 'z' : tail x
        sub a b c = do
          let gab = graph a b
          t <- lookup "XOR" gab
          u <- lookup "AND" gab
          let gtc = graph t c
          zc <- lookup "XOR" gtc
          guard $ zc == z
          v <- lookup "AND" gtc
          lookup "OR" $ graph u v

    Just c00 = lookup "AND" $ graph "x00" "y00"

    checkloop k cin =
      case check k cin of
        Just cout -> checkloop (succ k) cout
        Nothing   -> (k, cin, graph x y, graph y cin, graph cin x)
      where
        x = 'x' : drop 1 (show $ k + 100)
        y = 'y' : drop 1 x

    g = M.fromListWith (++) [((min a c, max a c), [(b, repl d)]) | (a,b,c,d) <- p2]

    repl "nqk" = "z07"
    repl "z07" = "nqk"
    repl "fgt" = "pcp"
    repl "pcp" = "fgt"
    repl "fpq" = "z24"
    repl "z24" = "fpq"
    repl "srn" = "z32"
    repl "z32" = "srn"
    repl sig = sig

part2ans = intercalate "," $ sort
  [ "nqk", "z07"
  , "fgt", "pcp"
  , "fpq", "z24"
  , "srn", "z32"
  ]
