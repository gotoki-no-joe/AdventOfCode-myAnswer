import Data.List.Split
import Data.Char
import Data.List

import Debug.Trace

runner i f = do
  pvs <- map parse . lines <$> readFile i
  print $ f pvs

-- クソ、またコンマ区切りかよ！マイナスが消されちまう！

parse :: String -> [Integer]
parse l = map read $ wordsBy p l
  where
    p '-' = False
    p c = not $ isDigit c

test1 = runner "sample.txt" $ part1 (7, 27)
main1 = runner "input.txt" $ part1 (2*10^14, 4*10^14)

part1 (lb, ub) pvs = length [() | pv:qus <- tails pvs, qu <- qus, prop1 pv qu]
  where
    prop1 a1@(x1:y1:_z1:vx:vy:_vz:_) a2@(x2:y2:_z2:ux:uy:_uz:_) = -- traceShow (a1, a2, den, fromIntegral px / fromIntegral den, fromIntegral py / fromIntegral den) $
        case signum den of
          0 -> False
          1 -> num >= 0 && c1 && c2 && scond
          -1 -> num <= 0 && d1 && d2 && scond
      where
        dx = x1 - x2
        dy = y1 - y2
        den = ux * vy - vx * uy
        num = uy * dx - ux * dy
        px = den * x1 + vx * num
        py = den * y1 + vy * num
        c1 = den * lb <= px && px <= den * ub
        c2 = den * lb <= py && py <= den * ub
        d1 = den * ub <= px && px <= den * lb
        d2 = den * ub <= py && py <= den * lb
        scond = signum (den * dx + num * vx) * signum ux * signum den >= 0

{-
パート1
z座標は無視して、
p t = (x1,y1) + t(vx,vy)
q s = (x2,y2) + s(ux,uy)
の交点が lb≦x, y≦ub に入っているか判定せよと。

x1 - x2 = sux - tvx
y1 - y2 = suy - tvy

uy dx = s ux uy - t vx uy
ux dy = s ux uy - t ux vy

uy dx - ux dy = t (ux vy - vx uy)
t = (uy dx - ux dy) / (ux vy - vx uy) = num / den

lb ≦ p t = (x1, y1) + (vx, vy) * num / den ≦ ub
den lb ≦ den (x1, y1) + num (vx, vy) ≦ den ub

den (lb - (x1, y1)) ≦ num (vx, vy) ≦ den (ub - (x1, y1)) -- これはもういい。

これで整数の範囲で対処できる？
input.txt はみんな15桁の数だけど、
maxBound :: Int = 9223372036854775807
は20桁の数。
Intで答えを出しつつ、Integerでもやってみて、違いがないか突き合わせる？
いやこれ最初から無理だわ。
かといってdoubleでやるとダメなパターンなんじゃないかなぁ。

そして、denが負の値になるパターン！なるほどね。

おおっとさらに！futureという条件が！
交差する点の位置が、s,tともに負でない、が必要なの？ありゃま。
t = num / den なので、denとnumの符号が等しいかnumは0、でtはよし。

dx = sux - tvx
s ux = dx + tvx

den * ux * s = den * dx + num * vx = RHS
ux == 0 || signum RHS * signum den * signum ux/= -1
signum rhs * signum ux = signum den or 0
-}

{-
パート2

今度は全員一斉に、しかも時刻を気にして、一直線に撃ち抜くような線を見つけろと。
2つの点が固定されたら直線が決まるけれど、それぞれを通過する時刻が異なるから固定できないんですけど。

うーん？
弾丸と雹のそれぞれが通過する直線が与えられている。
弾丸はそれらと必ずどこかで交差する直線となっている。パート1の結果から、
他の全員と交差している雹はないので、弾丸は実はどれかの雹そのもの、ということもない。
予備調査だった。

さて、直線どうしがどこかで交差するということは、その2直線はねじれの位置ではなく、平面を定義する。
二つの雹の直線A,Bが交差しているとする。
弾丸の直線Xは、AともBとも交差しているものである。
AとBが交差しているとき、ABは一つの平面をなし、Xはこの平面上にあるか、
AとBの交点を貫く、平面上にない直線という特例のいずれかである。
しかし、雹どうしが衝突することはなかった、と追加ヒントがあるので、
弾丸がAの雹とBの雹が衝突した瞬間を一石二鳥で貫く、ということもない。
交点を通過する時刻が異なるなら、なおのこと弾丸は多くて片方にしか当たらない。
なのでそういうことを考える必要はなく、交差する直線がなす平面上にXはある。

ということは、ABのような交差する直線がなす平面どうしは、どれもXの直線で交わる。
同一でない平面を二つ見つければそれで直線Xそのものは発見できてしまうし、
他の組み合わせでも同じ直線に至るなら確信が得られる。

直線が見つかったなら、全ての雹の直線と交差する点を見つけられる。
交差する点が見つかったなら、その雹がその点を通る時刻がわかる。
時刻と座標が二つ判れば、時刻0のときに弾丸がいるべき地点と、射出速度もわかる。

最後に、それが本当に全ての点を正しい時刻に撃ち抜くかを検証できる。

もっと力業でできないかな。
撃ち抜く時刻が整数とは限らないので無理か？だね。

直線のベクトル表現：p + tv
2直線を通る平面のベクトル表現？基準点を通り、法線ベクトルに垂直な位置ベクトル
https://wiis.info/math/linear-algebra/vector/equation-of-plane/
直線 p1 + t v1 と p2 + s v2 が交わるとき、この2直線を通る平面は
p1 + t v1 + s v2 で表される点からなる。なるほど。
法線ベクトル n を v1 × v2 で作れば、(x - p)・n = 0 と表せる。法線標準形というそうな。


この表現で、二つの平面の交わる直線はどうやる。
https://wiis.info/math/linear-algebra/vector/intersection-of-two-planes/
連立方程式を解くしかないのか。
なんか昔どこかで作ったぞ。HaskellならRationalを使えば簡単に無限精度だが。

((x,y,z) - p1)・n12 = 0 という平面の式を展開すると
n12x * x + n12y * y + n12z * z = p1x * n12x + p1y * n12y + p1z * n12z (E1)
という一本の式になる。
もう一つ同様の平面を p3, n34 で定義したものが
n34x * x + n34y * y + n34z * z = p3x * n34x + p3y * n34y + p3z * n34z (E2)
となる。
これで連立方程式にはなったが、変数が足らないので一意の解があるわけではなく、直線の方程式になっているはず。

その直線が、X,Y,Zいずれかの座標軸と平行になっていない限り、X=0, Y=0, Z=0 という3平面との切片がある。


もっと簡単だ。
法線標準形の二つの平面があるとき、この2平面の交点がなす直線の法線ベクトルは、
n1 とも n2 とも垂直（どちらの平面にも含まれるから）なので、それは n1×n2 で作れる。
P1 : (X - p1)・n12 = 0
P2 : (X - p3)・n34 = 0
n1234 = n12×n34 として、X = p1234 + t n1234 が直線のベクトル表現。ただしp1234がどこか不明。
P1上にあるので代入して (p1234 + t n1234)・n12 = p1・n12
n1234⊥n12より p1234・n12 = p1・n12
P2についても同様にして p1234・n34 = p3・n34
p1234 = (x,y,z) として
n12x * x + n12y * y + n12z * z = p1・n12
n34x * x + n34y * y + n34z * z = p3・n34
式が足らない、この2式を満たすどこでもいい。n12x ≠ 0 ならば (p1・n12/n12x, 0, 0) = (p3・n34/n34x, 0, 0)
n12y ≠ 0 ならば (0, p1・n12/n12y, 0), n12z ≠ 0 ならば (0, 0, p1・n12/n12z)
どれでもないとき、n12 = (0,0,0) それは法線ベクトルでない。
それってn1=kn2とかのときだよね。

こうして p1234 + t n1234 = X で直線が作れた。
これはあらゆる pk + sk vk = X と交差している。知りたいのは交差する位置の t の値だけ。skの値は要らない。
それが二つわかれば、X | t=0 が求める初期位置。
しかし変数が二つあるとまずい。tとskは原点は違うがオフセットは同じなので、
p1234 + (t - t0) n1234 = pk + t vk
とする。いやこれ変数s消して変数t0増えちゃってるから同じか？

単なる2直線の交点だから、一つに決まるはずだよな？存在するなら。
p1x + t v1x = p2x + s v2x
p1y + t v1y = p2y + s v2y
p1z + t v1z = p2z + s v2z

v2y p1x + t v2y v1x = v2y p2x + s v2y v2x
v2x p1y + t v2x v1y = v2x p2y + s v2x v2y
(v2y v1x - v2x v1y) t = (v2y p2x - v2x p2y) - (v2y p1x - v2x p1y) = v2y (p2x - p1x) - v2x (p2y - p1y)

ちがう、一つでいい。直線X(仮)で交点を表すtと、直線Aで交点を表すsが見つかったとき、
交差するのは時刻sでないとAの雹はそこにいないので、
直線X(真)では交点はsで表されるはず。なのでtでなくsの方を求めて、
t = s となるような出発地点を逆算する。

v1y p1x + t v1y v1x = v1y p2x + s v1y v2x
v1x p1y + t v1x vy1 = v1x p2y + s v1x v2y

v1y p1x - v1x p1y - v1y p2x + v1x p2y = s (v1y v2x - v1x v2y)
s = (v1y (p1x - p2x) - v1x(p1y - p2y)) / (v1y v2x - v1x v2y)

p1234 + (s + t0) n1234 = pk + s vk いま s はもう得られたので
p1234 + t0 n1234 + s n1234 = pk + s vk
こうすると、真の出発地点は p1234 + t0 n1234 なので
p1234 + t n1234 = pk + s vk - s n1234
と求められる。

結局？
交わる雹の2線 p1 + t v1, p2 + s v2 から、両方を通る平面 (x - p1)・n12 = 0 (n12 = v1×v2) を作る。
別の2線 p3 + t v3, p4 + t v4 からも、(x - p3)・n34 = 0 (n34 = v3×v4) を作る。
この2平面の交点の直線は、方向ベクトルが vv = n12×n34 で、（ここで vv=(0,0,0)のときは失敗）
X・n12 = p1・n12 = RHS の特殊解ppが (RHS/n12x,0,0),(0,RHS/n12y,0),(0,0,RHS/n12z) のどれかで得られる。
そして交線は pp + (t0 + t) vv = X となる。ただしt0は不明。
これは全ての雹の線とおのおの異なる時刻tで交わることから、t0を求めて pp + t0 vv を求めたい。
pp + (t0 + t) vv = p1 + t v1
(vv - v1) t + vv t0 = p1 - pp これは変数t,t0、等式はX,Y,Zの3本の連立一次方程式なのでt0は求められる。というか多い。

パート1からRationalを使うようにやり直そうか。
-}
