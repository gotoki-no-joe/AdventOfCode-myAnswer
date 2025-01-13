import Data.List

parse :: String -> [Int]
parse xs = map (read . (ws !!)) [3, 6, 13]
  where
    ws = words xs

runner i f = readFile i >>= print . f . map parse . lines

part1 time ls = maximum $ map dist ls
  where
    dist [s,t,u] = sum $ take time $ cycle (replicate t s ++ replicate u 0)

test1 = runner "test.txt"  (part1 1000)
main1 = runner "input.txt" (part1 2503)

part1a time ls = maximum $ map dist ls
  where
    dist [s,t,u] = s * (q * t + min t r)
      where
        (q,r) = divMod time (t + u)

main1a = runner "input.txt" (part1a 2503)

part2 time ls =
    maximum $             -- 最高得点
    map sum $ transpose $ -- トナカイごとに足し合わせ
    map point2top $       -- 首位にポイント付与
    take time $           -- レース時間で打ち切り
    transpose $           -- 毎秒ごとに、各トナカイの位置のリスト、のリスト
    map genDists ls       -- トナカイごとに、毎秒の位置のリスト、のリスト
  where
    genDists [s,t,u] = scanl1 (+) $ cycle (replicate t s ++ replicate u 0)
    point2top ds = [if d == m then 1 else 0 | d <- ds]
      where
        m = maximum ds

test2 = runner "test.txt"  (part2 1000)
main2 = runner "input.txt" (part2 2503)
