{-
前にもあったなこんな感じの。
-}

import Data.Char
import Data.List
import Data.Bits

{-
base2 :: Int -> BinStream
base2 = take 4 . unfoldr step
  where
    step n = Just $ (\(a,b) -> (b,a)) $ divMod n 2
-}

hex2bin :: Char -> BinStream
hex2bin h = [ if testBit n i then 1 else 0 | i <- [3,2..0]]
  where
    n = digitToInt h

bs2i :: BinStream -> Int
bs2i = loop 0
  where
    loop acc [] = acc
    loop acc (b:bs) = loop (acc * 2 + b) bs

sample1 = "D2FE28"
sample2 = "38006F45291200"
sample3 = "EE00D40C823060"
sample4 = "8A004A801A8002F478"
sample5 = "620080001611562C8802118E34"
sample6 = "C0015000016115A2E0802F182340"
sample7 = "A0016C880162017C3686B18A3D4780"

test1 :: String -> (Packet, BinStream)
test1 = readPacket . concatMap hex2bin

type BinStream = [Int]
type PacketHeader = (Int, Int) -- Version, Type
type Packet = (PacketHeader, PacketBody)
data PacketBody = Literal Int | Operator [Packet] deriving Show

readPacket :: BinStream -> (Packet, BinStream)
readPacket bs
  | t == 4 = (((v, t), Literal n), bs3)
  | otherwise = (((v, t), Operator ps), bs4)
  where
    (vb, bs1) = splitAt 3 bs
    v = bs2i vb
    (tb, bs2) = splitAt 3 bs1
    t = bs2i tb
    (n, bs3) = readDecimal bs2
    (ps, bs4) = readOperator bs2

readDecimal :: BinStream -> (Int, BinStream)
readDecimal bs = (bs2i as, bs1)
  where
    (as,bs1) = loop bs
    loop :: BinStream -> (BinStream, BinStream)
    loop (0:bs) = splitAt 4 bs
    loop (1:bs) = (as ++ as1, bsn)
      where
        (as, bs1) = splitAt 4 bs
        (as1, bsn) = loop bs1

readOperator :: BinStream -> ([Packet], BinStream)
readOperator (0:bs) = (ps, bs3)
  where
    (lb, bs1) = splitAt 15 bs
    len = bs2i lb
    (bs2, bs3) = splitAt len bs1
    ps = unfoldr step bs2
    step [] = Nothing
    step bs = Just $ readPacket bs
readOperator (1:bs) = (map fst pbs, snd $ last pbs)
  where
    (nb, bs1) = splitAt 11 bs
    num = bs2i nb
    pbs = take num $ unfoldr step bs1
    step bs = let pbs1 = readPacket bs in Just (pbs1, snd pbs1)

compute1 :: Packet -> Int
compute1 ((v,_), pb) =
  case pb of
    Literal _ -> v
    Operator ps -> v + sum (map compute1 ps)

main1 :: String -> Int
main1 = compute1 . fst . readPacket . concatMap hex2bin

input = "220D790065B2745FF004672D99A34E5B33439D96CEC80373C0068663101A98C406A5E7395DC1804678BF25A4093BFBDB886CA6E11FDE6D93D16A100325E5597A118F6640600ACF7274E6A5829B00526C167F9C089F15973C4002AA4B22E800FDCFD72B9351359601300424B8C9A00BCBC8EE069802D2D0B945002AB2D7D583E3F00016B05E0E9802BA00B4F29CD4E961491CCB44C6008E80273C393C333F92020134B003530004221347F83A200D47F89913A66FB6620016E24A007853BE5E944297AB64E66D6669FCEA0112AE06009CAA57006A0200EC258FB0440010A8A716A321009DE200D44C8E31F00010887B146188803317A3FC5F30056C0150004321244E88C000874468A91D2291802B25EB875802B28D13550030056C0169FB5B7ECE2C6B2EF3296D6FD5F54858015B8D730BB24E32569049009BF801980803B05A3B41F1007625C1C821256D7C848025DE0040E5016717247E18001BAC37930E9FA6AE3B358B5D4A7A6EA200D4E463EA364EDE9F852FF1B9C8731869300BE684649F6446E584E61DE61CD4021998DB4C334E72B78BA49C126722B4E009C6295F879002093EF32A64C018ECDFAF605989D4BA7B396D9B0C200C9F0017C98C72FD2C8932B7EE0EA6ADB0F1006C8010E89B15A2A90021713610C202004263E46D82AC06498017C6E007901542C04F9A0128880449A8014403AA38014C030B08012C0269A8018E007A801620058003C64009810010722EC8010ECFFF9AAC32373F6583007A48CA587E55367227A40118C2AC004AE79FE77E28C007F4E42500D10096779D728EB1066B57F698C802139708B004A5C5E5C44C01698D490E800B584F09C8049593A6C66C017100721647E8E0200CC6985F11E634EA6008CB207002593785497652008065992443E7872714"

run1 = main1 input

{-
*Main> test1 sample1
(((6,4),Literal 2021),[0,0,0])
*Main> test1 sample2
(((1,6),Operator [((6,4),Literal 10),((2,4),Literal 20)]),[0,0,0,0,0,0,0])
*Main> test1 sample3
(((7,3),Operator [((2,4),Literal 1),((4,4),Literal 2),((1,4),Literal 3)]),[0,0,0,0,0])
*Main> main1 sample4
16
*Main> main1 sample5
12
*Main> main1 sample6
23
*Main> main1 sample7
31
*Main> run1
989
-}

-- 後半、意外性がないな。

eval :: Packet -> Int
eval ((v,t), pb) =
  case pb of
    Literal n -> n
    Operator ps -> [sum, product, minimum, maximum, undefined, gt, lt, eq] !!  t $ map eval ps
  where
    gt [a,b] = if a > b then 1 else 0
    lt [a,b] = if a < b then 1 else 0
    eq [a,b] = if a == b then 1 else 0

main2 :: String -> Int
main2 = eval . fst . readPacket . concatMap hex2bin

sample8 = "C200B40A82"
sample9 = "04005AC33890"
sample10 = "880086C3E88112"
sample11 = "CE00C43D881120"
sample12 = "D8005AC2A8F0"
sample13 = "F600BC2D8F"
sample14 = "9C005AC2F8F0"
sample15 = "9C0141080250320F1802104A08"

{-
*Main> main2 sample10
7
*Main> main2 sample11
9
*Main> main2 sample12
1
*Main> main2 sample13
0
*Main> main2 sample14
0
*Main> main2 sample15
1
*Main> main2 input
7936430475134
-}
