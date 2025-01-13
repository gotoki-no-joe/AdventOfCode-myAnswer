import qualified Data.Map as M

txt0 = "^WNE$" -- 3
txt1 = "^ENWWW(NEEE|SSE(EE|N))$" -- 10
txt2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" -- 18
txt3 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" -- 23
txt4 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" -- 31

runner f = readFile "input.txt" >>= print . f . head . lines

test1 = ([3,10,18,23,31], map part1 [txt0, txt1, txt2, txt3, txt4])

main1 = runner part1
main2 = runner part2

part1 l = lengthRE $ parse l

{-
任意の正規表現ではなく、(WNSE|) みたいな戻ってくるものだけが途中に挟まれて、
それよりも長い経路がかならずその後にあって、最後だけ枝分かれして終わり、
みたいな制約がかかっているようだ。
はっきり書いてないからアレだけど。input.txt見てみると、4文字系列に限定されない、|) が297個からあった。
概ね、一つの分岐の文字数を数えたらいいのだと思うけど、どうしたものかね。
-}

data REEntry = Lit Char | Choice [RE] deriving Show
type RE = [REEntry]

parse :: String -> RE
parse ('^':l) =
  case get l of
    ("$", re) -> re
    res -> error $ show res
  where
    get (c:cs) | elem c "NEWS" = (Lit c :) <$> get cs
    get ('(':cs) = choices [] cs
    get cs = (cs,[])
    choices res cs =
      case get cs of
        (')':cs1, re) -> (Choice (reverse (re : res)) :) <$> get cs1
        ('|':cs1, re) -> choices (re:res) cs1

lengthRE :: RE -> Int
lengthRE re = sum $ map f re
  where
    f (Lit _) = 1
    f (Choice res)
      | last ls == 0 = 0
      | otherwise    = maximum ls
      where
        ls = map lengthRE res

{-
パート2

上のロジックであってたということは、グルリの解釈もいいってことだね。
で、何を聞かれているの？
1000以上の距離がある、全ての部屋の個数、かな？
1000手前ギリギリに「ぐるり」があると狂うんで気になるな。

-}

countFar0 :: RE -> Int
countFar0 res = loop (- 1000) res
  where
    loop _ [] = 0
    loop d (Lit _:res) = (if d >= 0 then succ else id) $ loop (succ d) res
    loop d [Choice res] = sum $ map (loop d) res
    loop d (Choice res : res1) | any null res = loop d res1

part2 l = countFar $ parse l

{-
ghci> main2
8114
your answer is too low.

input.txtの総文字数が14326で、part1が4000ならそんなもんじゃないかと思ったが、
グルリまわりで違いができるパターンかな。
グルリが一周しているなら、その長さの半分だけを調べたらいい。
グルリが、Litだけで構成されているか確認しようか。
-}

checkCirc = f . parse
  where
    f (Lit _:res) = f res
    f [Choice res] = all f res
    f (Choice res:res1) = f res1 && any null res && all (all isLit) res
    f [] = True
    isLit (Lit _) = True
    isLit _ = False

-- いいようだ。

countFar00 :: RE -> Int
countFar00 res = loop (- 1000) res
  where
    loop _ [] = 0
    loop d (Lit _:res) = (if d >= 0 then succ else id) $ loop (succ d) res
    loop d [Choice res] = sum $ map (loop d) res
    loop d (Choice res : res1) | any null res = loop d res1 + sum (map (loop d) $ concatMap halve res)

    halve xs = [as, reverse bs]-- 本当はbsの個々のLitの向きも逆にしたい
      where
        (as,bs) = splitAt (div (length xs) 2) xs

{-
ghci> main2
9376
your answer is too high.

えー？
あー、ぐるり、じゃなくて、同じ道を引き返してくるんだ。だから「直す」まえが正しかった。
-}

countFar :: RE -> Int
countFar res = loop 1 res
  where
    loop _ [] = 0
    loop d (Lit _:res) = (if d >= 1000 then succ else id) $ loop (succ d) res
    loop d [Choice res] = sum $ map (loop d) res
    loop d (Choice res : res1) | any null res = loop d res1 + sum (map (loop d . halve) res)

    halve xs = take (div (length xs) 2) xs

{-
ghci> main2
8745
your answer is too high.

あるえー？
1000ドア以上を開ける、は、1000個めから数えていいんだよね。

イラつくので、全ての点の座標と距離を書き出してしまうか。
-}

part2a l = length $ filter (1000 <=) $ M.elems m
  where
    re = parse l
    m = M.fromListWith min $ makeMap (0,0) 0 re
    makeMap _ _ [] = []
    makeMap ij d (Lit dir : re) = (ij1, d1) : makeMap ij1 d1 re
      where
        ij1 = move ij dir
        d1 = succ d
    makeMap ij d [Choice res] = concatMap (makeMap ij d) res
    makeMap ij d (Choice res:res1) | any null res = concatMap (makeMap ij d) (res1 : res)

    move (i,j) 'N' = (pred i, j)
    move (i,j) 'E' = (i, succ j)
    move (i,j) 'W' = (i, pred j)
    move (i,j) 'S' = (succ i, j)

{-
ghci> runner part2a
8281

正解したけど、逆に前ので違うのはどうなってるのと。
「グルリ」が、(NNSS|NEWS|) みたいなのが混じってるとかか？
なんにせよ、「優しい」が中途半端で、そんなら問題文に全部書けよって感じだ。
-}
