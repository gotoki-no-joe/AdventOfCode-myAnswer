import Text.Parsec
import Control.Monad

runner i f = do
  s <- readFile i
  let ans = f s
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

test2 = runner "sample2.txt" part2
main2 = runner "input.txt" part2

sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

part1 :: String -> Int
part1 s = ans
  where
    Right ans = parse parser1 "" $ init s

parser1 = sum . concat <$> many (try mul <|> other)

other :: Stream s m Char => ParsecT s u m [Int]
other = anyChar >> return []

mul :: Stream s m Char => ParsecT s u m [Int]
mul = do
  string "mul("
  a1 <- many1 digit
  char ','
  a2 <- many1 digit
  char ')'
  when (length a1 > 3 || length a2 > 3) $ fail "too long"
  return [read a1 * read a2]

part2 :: String -> Int
part2 s = ans
  where
    Right ans = parse parser2on "" $ init s

parser2off = try (string "do()" >> parser2on) <|> (anyChar >> parser2off) <|> (eof >> return 0)
parser2on = try (string "don't()" >> parser2off) <|> try mul2 <|> (anyChar >> parser2on) <|> (eof >> return 0)

mul2 = do
  string "mul("
  a1 <- many1 digit
  char ','
  a2 <- many1 digit
  char ')'
  when (length a1 > 3 || length a2 > 3) $ fail "too long"
  let val = read a1 * read a2
  rest <- parser2on
  return $ val + rest

{-
正規表現より強力な parsec を使う版
正直めちゃくちゃ疲れた
-}
