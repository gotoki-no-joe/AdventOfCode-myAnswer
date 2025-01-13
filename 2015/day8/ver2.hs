runner i f = readFile i >>= print . f . lines

part1 :: [String] -> Int
part1 = sum . map f
  where
    f ('\\':'x':_:_:xs) = 3 + f xs
    f ('\\':'\\':xs) = succ $ f xs
    f ('\\':'\"':xs) = succ $ f xs
    f (_:xs) = f xs
    f "" = 2

test1 = runner "test.txt" part1
main1 = runner "input.txt" part1

part2 :: [String] -> Int
part2 = sum . map f
  where
    f l = 2 + length (filter (flip elem "\"\\") l)

test2 = runner "test.txt" part2
main2 = runner "input.txt" part2
