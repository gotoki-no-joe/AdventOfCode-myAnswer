# 12日目：N体問題 #

木星の近くの宇宙空間はとても安全な場所ではありません。

頭のおかしくなりそうな[大赤斑](https://ja.wikipedia.org/wiki/%E5%A4%A7%E8%B5%A4%E6%96%91)、
強烈な放射、
そして
[たくさんの月](https://ja.wikipedia.org/wiki/%E6%9C%A8%E6%98%9F%E3%81%AE%E8%A1%9B%E6%98%9F#%E8%A1%9B%E6%98%9F%E3%81%AE%E4%B8%80%E8%A6%A7)
が公転していることに注意する必要があります。
4つの最大の月、イオ、エウロパ、ガニメデ、カリストを追跡することから始めることにしました。

手短なスキャンの後、**それぞれの月の位置**（パズル入力）を算出しました。
あなたは月との衝突を避けるためだけに、**それらの動きをシミュレートする**必要があります。

それぞれの月は3次元位置(`x`,`y`,`z`)および3次元速度を持っています。
それぞれの月の位置はスキャンで指定されています。
それぞれの月の速度の`x`,`y`,`z`は`0`で開始します。

月の動きは**タイムステップ**でシミュレートします。
各タイムステップでは、最初にすべての月の速度を**重力**を適用することにより更新します。
そして、すべての月の速度が更新された後に、**速度**を適用することによってすべての月の位置を更新します。
すべての位置が更新されると、時間は1ステップ進みます。

**重力**を適用するには、月のすべての対を考えます。
それぞれの軸（`x`,`y`,`z`）について、月どうしが互いに引き合うように、
それぞれの月の速度は**ちょうど`+1`または`-1`だけ**変化します。
例えば、ガニメデの`x`座標が3でカリストの`x`座標が5である場合、
ガニメデの`x`速度は **`+1`変化**し（5 > 3だから）、
カリストの`x`速度は **`-1`変化**し（3 < 5だから）ます。
ただし、ある軸の座標が同じ場合、その軸の速度はその月の組に対して**変化しません**。

すべての重力が適用された後、**速度**を適用します：
単にそれぞれの月の位置に自身の速度を足しこみます。
例えば、エウロパの位置が`x=1, y=2, z=3`で速度が`x=-2, y=0, z=3`の場合、
新しい位置は`x=-1, y=2, z=6`になります。
このプロセスは月の速度を変更しません。

たとえば、スキャンによって明らかになった位置が次のとおりだったとします。

```
<x=-1, y=  0, z= 2>
<x= 2, y=-10, z=-7>
<x= 4, y= -8, z= 8>
<x= 3, y=  5, z=-1>
```

これらの衛星の動きをシミュレートすると、以下が生成されます。
（pos:位置, vel:速度）

```
0 ステップ後:
pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>

1 ステップ後:
pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>

2 ステップ後:
pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>

3 ステップ後:
pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>

4 ステップ後:
pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>

5 ステップ後:
pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>

6 ステップ後:
pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>

7 ステップ後:
pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>

8 ステップ後:
pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>

9 ステップ後:
pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>

10 ステップ後:
pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
```

次に、**星系内の総エネルギー**を計算しておくと役に立つでしょう。
ひとつの月の総エネルギーは、その**位置エネルギー**に**運動エネルギー**を掛けたものです。
月の**位置エネルギー**はその`x`,`y`,`z`座標の絶対値の和です。
月の**運動エネルギー**は速度座標の絶対値の合計です。
下に、行ごとに月の位置エネルギー(pot)、運動エネルギー(kin)、総エネルギーの計算を示しています。

```
10 ステップ後のエネルギー:
pot: 2 + 1 + 3 =  6;   kin: 3 + 2 + 1 = 6;   total:  6 * 6 = 36
pot: 1 + 8 + 0 =  9;   kin: 1 + 1 + 3 = 5;   total:  9 * 5 = 45
pot: 3 + 6 + 1 = 10;   kin: 3 + 2 + 3 = 8;   total: 10 * 8 = 80
pot: 2 + 0 + 4 =  6;   kin: 1 + 1 + 1 = 3;   total:  6 * 3 = 18
総エネルギーの和: 36 + 45 + 80 + 18 = 179
```

上記の例では、10ステップ後のすべての月の合計エネルギーを合計すると、
星系の合計エネルギー`179`が求まります。

次に2番目の例を示します。

```
<x=-8, y=-10, z= 0>
<x= 5, y=  5, z=10>
<x= 2, y= -7, z= 3>
<x= 9, y= -8, z=-3>
```

100ステップのシミュレーションの10ステップごとを示します：

```
0 ステップ後:
pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>
pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>
pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>

10 ステップ後:
pos=<x= -9, y=-10, z=  1>, vel=<x= -2, y= -2, z= -1>
pos=<x=  4, y= 10, z=  9>, vel=<x= -3, y=  7, z= -2>
pos=<x=  8, y=-10, z= -3>, vel=<x=  5, y= -1, z= -2>
pos=<x=  5, y=-10, z=  3>, vel=<x=  0, y= -4, z=  5>

20 ステップ後:
pos=<x=-10, y=  3, z= -4>, vel=<x= -5, y=  2, z=  0>
pos=<x=  5, y=-25, z=  6>, vel=<x=  1, y=  1, z= -4>
pos=<x= 13, y=  1, z=  1>, vel=<x=  5, y= -2, z=  2>
pos=<x=  0, y=  1, z=  7>, vel=<x= -1, y= -1, z=  2>

30 ステップ後:
pos=<x= 15, y= -6, z= -9>, vel=<x= -5, y=  4, z=  0>
pos=<x= -4, y=-11, z=  3>, vel=<x= -3, y=-10, z=  0>
pos=<x=  0, y= -1, z= 11>, vel=<x=  7, y=  4, z=  3>
pos=<x= -3, y= -2, z=  5>, vel=<x=  1, y=  2, z= -3>

40 ステップ後:
pos=<x= 14, y=-12, z= -4>, vel=<x= 11, y=  3, z=  0>
pos=<x= -1, y= 18, z=  8>, vel=<x= -5, y=  2, z=  3>
pos=<x= -5, y=-14, z=  8>, vel=<x=  1, y= -2, z=  0>
pos=<x=  0, y=-12, z= -2>, vel=<x= -7, y= -3, z= -3>

50 ステップ後:
pos=<x=-23, y=  4, z=  1>, vel=<x= -7, y= -1, z=  2>
pos=<x= 20, y=-31, z= 13>, vel=<x=  5, y=  3, z=  4>
pos=<x= -4, y=  6, z=  1>, vel=<x= -1, y=  1, z= -3>
pos=<x= 15, y=  1, z= -5>, vel=<x=  3, y= -3, z= -3>

60 ステップ後:
pos=<x= 36, y=-10, z=  6>, vel=<x=  5, y=  0, z=  3>
pos=<x=-18, y= 10, z=  9>, vel=<x= -3, y= -7, z=  5>
pos=<x=  8, y=-12, z= -3>, vel=<x= -2, y=  1, z= -7>
pos=<x=-18, y= -8, z= -2>, vel=<x=  0, y=  6, z= -1>

70 ステップ後:
pos=<x=-33, y= -6, z=  5>, vel=<x= -5, y= -4, z=  7>
pos=<x= 13, y= -9, z=  2>, vel=<x= -2, y= 11, z=  3>
pos=<x= 11, y= -8, z=  2>, vel=<x=  8, y= -6, z= -7>
pos=<x= 17, y=  3, z=  1>, vel=<x= -1, y= -1, z= -3>

80 ステップ後:
pos=<x= 30, y= -8, z=  3>, vel=<x=  3, y=  3, z=  0>
pos=<x= -2, y= -4, z=  0>, vel=<x=  4, y=-13, z=  2>
pos=<x=-18, y= -7, z= 15>, vel=<x= -8, y=  2, z= -2>
pos=<x= -2, y= -1, z= -8>, vel=<x=  1, y=  8, z=  0>

90 ステップ後:
pos=<x=-25, y= -1, z=  4>, vel=<x=  1, y= -3, z=  4>
pos=<x=  2, y= -9, z=  0>, vel=<x= -3, y= 13, z= -1>
pos=<x= 32, y= -8, z= 14>, vel=<x=  5, y= -4, z=  6>
pos=<x= -1, y= -2, z= -8>, vel=<x= -3, y= -6, z= -9>

100 ステップ後:
pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>
pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>
pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>
pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>

100 ステップ後のエネルギー:
pot:  8 + 12 +  9 = 29;   kin: 7 +  3 + 0 = 10;   total: 29 * 10 = 290
pot: 13 + 16 +  3 = 32;   kin: 3 + 11 + 5 = 19;   total: 32 * 19 = 608
pot: 29 + 11 +  1 = 41;   kin: 3 +  7 + 4 = 14;   total: 41 * 14 = 574
pot: 16 + 13 + 23 = 52;   kin: 7 +  1 + 1 =  9;   total: 52 *  9 = 468
総エネルギーの和: 290 + 608 + 574 + 468 = 1940
```

あなたのスキャンで与えられた月を1000ステップシミュレートした後の
**星系の総エネルギーはどれだけですか？**

# パート2 #

この宇宙空間を漂流しているものたちから、あなたは宇宙の性質について疑問を抱きました。
歴史は本当に繰り返されるだろうか？
あなたは月が以前の状態に戻るかどうか気になりました。

すべての月の**位置と速度**が前の時点に正確に一致するまでに必要な**ステップ数**を求めてください。

たとえば、上記の最初の例は前の時点に完全に一致するまでに2772ステップかかります。
この星系は最終的に初期状態に戻ります。

```
0 ステップ後:
pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>

2770 ステップ後:
pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>

2771 ステップ後:
pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>

2772 ステップ後:
pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
```

もちろん、宇宙は繰り返す前に**非常に長い間**続くかもしれません。
上記の2番目の例を再掲します。

```
<x=-8, y=-10, z= 0>
<x= 5, y=  5, z=10>
<x= 2, y= -7, z= 3>
<x= 9, y= -8, z=-3>
```

この初期位置の組は、前の状態を繰り返す前に4686774924ステップかかります！
明らかに、**宇宙をシミュレートするためのより効率的な方法**を見つける必要があるでしょう。

前の状態と完全に一致する最初の状態に到達するには**何ステップかかりますか？**