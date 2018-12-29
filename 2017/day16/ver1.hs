import Data.List.Split
import qualified Data.Map as M

main = do
  fi <- readFile "input.txt"
  let mvs = parse 16 $ head $ lines fi
  let ans1 = foldl (flip step) ['a'..'p'] mvs
  print ans1
  let foundloop = findloop mvs ['a'..'p']
  print foundloop

data CMD = CS Int | CE Int Int | CP Char Char

parse :: Int -> String -> [CMD]
parse len cs = map inner css where
  css = splitOn "," cs
  inner ('s':ns) = CS (len - read ns)
  inner ('x':ns) = let (as,bs) = breakCh '/' ns in CE (read as) (read bs)
  inner ['p',a,'/',b] = CP a b

breakCh c xs = let (as, _:bs) = break (c ==) xs in (as,bs)

step :: CMD -> [Char] -> [Char]
step (CS n) ps = let (as,bs) = splitAt n ps in bs ++ as
step (CE i j) ps = [ f k c | (k,c) <- zip [0..] ps ] where
  f k c | k == i = ps !! j
        | k == j = ps !! i
        | True   = c
step (CP a b) ps = map g ps where
  g c | c == a = b
      | c == b = a
      | True   = c

exec cmds str = foldl (flip step) str cmds

test1 = exec (parse 5 "s1,x3/4,pe/b") "abcde"

-- foldl (flip step) ['a'..'e'] (parse 5 "s1,x3/4,pe/b")

{-
きっとどこかでループするということなのでしょう。
それを発見しよう。

並び順をキー、何番目に出現したかの番号を値とするマップを生やしていく。
-}

-- findloop :: [CMD] -> [Char] -> (Int,Int,String)
findloop cmds initial = loop M.empty initial 0 where
  loop m s k
    | M.member s m = (m M.! s, k, m)
    | otherwise  = loop (M.insert s k m) (exec cmds s) (succ k)

test2 = findloop (parse 5 "s1,x3/4,pe/b") "abcde"

{-
*Main> main
"lbdiomkhgcjanefp"
(0,56)

だから56回踊ると元に戻る。

*Main> 1000000000 `mod` 56
48

あと48回踊ればいい。
というかfindloopの中のmapで48に写しているものが答えだ！
-}

{-
*Main> main
"lbdiomkhgcjanefp"
(0,56,fromList [("abcdefghijklmnop",0),("anhpigkbodemjcfl",41),("apjhokfmdgbicnel",38),("apngliojebdkcfhm",37),("dcnifagjlkbphmoe",32),("dcnihgajlobefmkp",4),("dnciahfblpjogmek",46),("dncigfhblejkampo",18),("efckagodmpnlbjhi",45),("efjpilachnmgbokd",44),("ejkflpgnamhdcboi",48),("enbmhopklcgidjaf",7),("fcakihejpdombngl",27),("fjndoghaibelmcpk",42),("fkbapegmdhjincol",24),("fkchlipbojdengam",23),("gbcdphafijolmnke",28),("gejfkohmdabicnpl",10),("genalikjpbdochfm",9),("gnfeiaobkdpmjchl",13),("hcgoifpjedkmbnal",55),("hjndkafgibplmceo",14),("hobgepamdfjinckl",52),("hocfliebkjdpnagm",51),("icmlfdhekjnobapg",50),("icmlhdfpojnkbgea",22),("idegmlkcajbphnfo",33),("idkfmlpnhbjogcae",47),("idohmlenfbjkacgp",19),("idpamlocgjbefnhk",5),("inmladgkpbcejhof",8),("inmlgdaoebcpjfkh",36),("kaboilhngcmfjepd",2),("kanphfedmocljbgi",3),("kbpalofchmgdnjei",6),("kcjmgeoplnfidbha",21),("lbdikmofacjgnphe",29),("lbdiomkhgcjanefp",1),("ljdiempgfnbhckao",15),("ljdipmeahnbfcogk",43),("lmakdihjoncgebpf",26),("lmfediabpcnhojkg",12),("lmgodifjkncapbeh",54),("lmhpdigbecnfkjoa",40),("mbjlkpecianhodgf",11),("mbjloepcignfkdah",39),("mjblekonifcapdhg",53),("mjblpoknihcgedfa",25),("obeglkhcfmadnjpi",34),("ocjmapkelnhidbfg",49),("ogbkilfnacmhjped",30),("ognefhpdmkcljbai",31),("phcogakdmenlbjfi",17),("phjeilgcfnmabkod",16),("pjohleangmfdcbki",20),("pnbmfkeolcaidjgh",35)])
*Main> test1
"baedc"
*Main> test2
(0,4,fromList [("abcde",0),("baedc",1),("ceadb",2),("ecbda",3)])

大した数ではないので目で探しました。
-}
