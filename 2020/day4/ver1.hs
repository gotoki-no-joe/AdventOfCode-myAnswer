import Data.List.Split
import qualified Data.Set as S
import Data.Char

sample = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

type Pass = [(String,String)]

buildData :: String -> [Pass]
buildData s = map (map f) $ map concat $ wordsBy null $ map words $ lines s
  where
    f xs = (take 3 xs, drop 4 xs)

required = S.fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

valid :: Pass -> Bool
valid p = required `S.isSubsetOf` S.fromList (map fst p)

test1 = length $ filter valid $ buildData sample

phase1 = do
  co <- readFile "input.txt"
  print $ length $ filter valid $ buildData co
  print $ length $ filter valid2 $ buildData co

{-
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
-}

valid2 :: Pass -> Bool
valid2 p = valid p && all ff p

ff ("byr",s) = length s == 4 && 1920 <= y && y <= 2002 where y = read s :: Int
ff ("iyr",s) = length s == 4 && 2010 <= y && y <= 2020 where y = read s :: Int
ff ("eyr",s) = length s == 4 && 2020 <= y && y <= 2030 where y = read s :: Int
ff ("hgt",s)
  | s2 == "cm" = 150 <= h && h <= 193
  | s2 == "in" = 59 <= h && h <= 76
  | True = False
  where
    (s1,s2) = span isDigit s
    h = read s1 :: Int
ff ("hcl",'#':s) = length s == 6 && all (flip elem "0123456789abcdef") s
ff ("hcl",_) = False
ff ("ecl",s) = elem s ["amb","blu","brn","gry","grn","hzl","oth"]
ff ("pid",s) = length s == 9 && all isDigit s
ff ("cid",_) = True
ff _ = False

{-
*Main> test1
2
*Main> phase1
242
186
-}
