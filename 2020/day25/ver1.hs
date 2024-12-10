{-
既に解かれていたが、記録が失われていたので再挑戦。

サブジェクト番号sは固定で、
値を1から開始して *s mod 20201227 を繰り返して次の値になる、
そのstreamに対して、公開鍵が何個目の値か、がloop sizeなのでまずそれを求める。

これは片方のだけすれば十分。
で、違う方の公開鍵を、サブジェクト番号としてloop sizeだけ変換すれば答えがでる。
逆をやっても同じ値になると確認したいけどね。
というか、mod 20201227 なので、そのステップ数を全て記録した配列を作ってしまえる。
ただしそれはサブジェクト番号ごとに違うから、最大20,201,227回で計算をブン回すしかないか。
-}

import Data.List

sample, input :: (Int, Int)
sample = (5764801, 17807724)
input = (15733400, 6408062)

modBase :: Int
modBase = 20201227
step :: Int -> Int -> Int
step subj x = mod (subj * x) modBase

findLoopSize pubkey = length $ takeWhile (pubkey /=) pubkeyseq
pubkeyseq = iterate (step 7) 1

encrypt loopsize subj = (!! loopsize) $ iterate (step subj) 1

part1 (key, door) = (keyLoopSize, doorLoopSize, keyEncryptionKey, doorEncryptionKey)
  where
    keyLoopSize = findLoopSize key
    doorLoopSize = findLoopSize door
    keyEncryptionKey = encrypt1 keyLoopSize door
    doorEncryptionKey = encrypt1 doorLoopSize key

test1 = part1 sample
main1 = part1 input

{-
ghci> test1
(8,11,14897079,14897079)
ghci> main1
(3903333,10459425,16457981,*** Exception: stack overflow

は？片方で出たはいいけど、どうしてそういうことになる？意味分からん。

ちな

ghci> :m +Data.Numbers.Primes
ghci> isPrime 20201227
True

そして落ち着いて考えてみると、encrypt は 1 * subject^loopsize を mod modBase で計算しているだけだこれ。
-}

-- @gotoki_no_joe
powerish mul i a b = foldl' {-'-} mul i [p | (True, p) <- zip bs ps]
  where
    bs = map odd $ takeWhile (0 <) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a

encrypt1 loopsize subj = powerish mul 1 subj loopsize
  where
    mul x y = mod (x * y) modBase

{-
ghci> test1
(8,11,14897079,14897079)
ghci> main1
(3903333,10459425,16457981,16457981)

むしろloopsizeの方が近道がないので時間がかかって、後半は一瞬になった。ふふ。
-}
