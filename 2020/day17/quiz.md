# 17日目：コンウェイの立方体 #

フライトがゆっくりと空を漂うと、北極の神話情報局の妖精から連絡がありました。
彼らは、彼らの極秘の観測衛星の1つに搭載された、
機能不全に陥っている実験的エネルギー源のデバッグを手伝ってほしいと思っています。

その実験的エネルギー源は、最先端の技術に基づいています。
ポケットサイズに納められたコンウェイの立方体の集合体です。
(cf. https://marvel.fandom.com/wiki/Glossary:Pocket_Dimension)
問題があると聞いたら、見ずにはいられません。

ポケットサイズに、無限の3次元グリッドが納められています。
すべての3次元整数座標（x,y,z）にはそれぞれ、
**活性状態**または**非活性状態**のいずれかの立方体がひとつ存在します。

初期状態では、ほとんどすべての立方体が**非活性状態**で始まります。
これに対する唯一の例外は、小さな平らな領域（パズル入力）です。
この領域の立方体は、指定された**活性状態**(`#`)または**非活性状態**(`.`)状態で開始します。

次に、エネルギー源は6**周期**を実行することによって起動に進みます。

個々の立方体は、**隣接する**立方体のみを考慮します。
それはどの座標も差がたかだか1であるような他の26個の立方体のことです。
たとえば、x=1,y=2,z=3の立方体に対して、
x=2,y=2,z=2の立方体、x=0,y=2,z=3の立方体などが隣接しています。

1つの周期で、**すべて**の立方体は次の規則に従って**同時に**状態が変化します。

- 立方体が**活性状態**であり、また**ちょうど2個または3個**の活性状態にある立方体と隣接しているならば、
その立方体は**活性状態**を維持します。
それ以外の場合、立方体は**非活性状態**になります。
- 立方体が**非活性状態**であり、また**ちょうど3個**の活性状態にある立方体と隣接しているならば、
その立方体は**活性状態**に変化します。
それ以外の場合、立方体は**非活性状態**を維持します。

この実験的エネルギー源を担当するエンジニアはあなたに、ポケット空間をシミュレーションし、
6周期の起動プロセスの完了後の立方体の立体配置を突き止めることを望んでいます。

たとえば、次の初期状態について考えてみます。

~~~
.#.
..#
###
~~~

ポケット空間は3次元ですが、この初期状態はその中の2次元の小さなスライスを表しています。
（具体的には、この初期状態は3次元空間の3x3x1領域を定義します。）

この初期状態から数周期をシミュレートすると、次の配置が生成されます。
各周期の結果を、指定された各z座標のレイヤーごとに示しています。
（また、表示範囲は各周期の活性状態の立方体に従っています）。

~~~
周期開始前:

z=0
.#.
..#
###


1周期後:

z=-1
#..
..#
.#.

z=0
#.#
.##
.#.

z=1
#..
..#
.#.


2周期後:

z=-2
.....
.....
..#..
.....
.....

z=-1
..#..
.#..#
....#
.#...
.....

z=0
##...
##...
#....
....#
.###.

z=1
..#..
.#..#
....#
.#...
.....

z=2
.....
.....
..#..
.....
.....


3周期後:

z=-2
.......
.......
..##...
..###..
.......
.......
.......

z=-1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=0
...#...
.......
#......
.......
.....##
.##.#..
...#...

z=1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=2
.......
.......
..##...
..###..
.......
.......
.......
~~~

6周期の起動プロセス全てが完了した後、
**112**の立方体が**活性状態**になっています。

指定されたあなたの初期配置から始めて、6周期をシミュレーションしてください。
**第6周期の後、活性状態の立方体はいくつありますか？**

# パート2 #

何らかの理由で、シミュレーション結果が実験的エネルギー源エンジニアの予想と一致しません。
どうやら、ポケット空間は実際には3次元空間ではなく**4次元空間**のようです。

ポケット空間には無限の4次元グリッドが内包されています。
すべての4次元整数座標（x,y,z,w）にはそれぞれ、
やはり**活性状態**または**非活性状態**のいずれかの立方体（正確には超立方体）がひとつ存在します。

個々の立方体は、**隣接する**立方体のみを考慮します。
それはどの座標も差がたかだか1であるような他の80個の立方体のことです。
たとえば、x=1,y=2,z=3,w=4の立方体に対して、
x=2,y=2,z=3,w=3の立方体、x=0,y=2,z=3,w=4の立方体などが隣接しています。

ポケット空間の初期状態は、やはり小さな平らな領域で構成されています。
さらに、同じ周期的な更新の規則が引き続き適用されます。
各周期中で、各立方体の活性状態にある隣接する立方体の数を考慮してください。

たとえば、上記の例と同じ初期状態について考えてみます。
ポケット空間は4次元ですが、この初期状態はその中の2次元の小さなスライスを表しています。
（具体的には、この初期状態は4次元空間の3x3x1x1領域を定義します。）

この初期状態から数周期をシミュレーションした結果を以下に示します。
ここで、各周期の結果は指定された各zおよびw座標のレイヤーごとに示しています。

~~~
周期開始前:

z=0, w=0
.#.
..#
###


1周期後:

z=-1, w=-1
#..
..#
.#.

z=0, w=-1
#..
..#
.#.

z=1, w=-1
#..
..#
.#.

z=-1, w=0
#..
..#
.#.

z=0, w=0
#.#
.##
.#.

z=1, w=0
#..
..#
.#.

z=-1, w=1
#..
..#
.#.

z=0, w=1
#..
..#
.#.

z=1, w=1
#..
..#
.#.


2周期後:

z=-2, w=-2
.....
.....
..#..
.....
.....

z=-1, w=-2
.....
.....
.....
.....
.....

z=0, w=-2
###..
##.##
#...#
.#..#
.###.

z=1, w=-2
.....
.....
.....
.....
.....

z=2, w=-2
.....
.....
..#..
.....
.....

z=-2, w=-1
.....
.....
.....
.....
.....

z=-1, w=-1
.....
.....
.....
.....
.....

z=0, w=-1
.....
.....
.....
.....
.....

z=1, w=-1
.....
.....
.....
.....
.....

z=2, w=-1
.....
.....
.....
.....
.....

z=-2, w=0
###..
##.##
#...#
.#..#
.###.

z=-1, w=0
.....
.....
.....
.....
.....

z=0, w=0
.....
.....
.....
.....
.....

z=1, w=0
.....
.....
.....
.....
.....

z=2, w=0
###..
##.##
#...#
.#..#
.###.

z=-2, w=1
.....
.....
.....
.....
.....

z=-1, w=1
.....
.....
.....
.....
.....

z=0, w=1
.....
.....
.....
.....
.....

z=1, w=1
.....
.....
.....
.....
.....

z=2, w=1
.....
.....
.....
.....
.....

z=-2, w=2
.....
.....
..#..
.....
.....

z=-1, w=2
.....
.....
.....
.....
.....

z=0, w=2
###..
##.##
#...#
.#..#
.###.

z=1, w=2
.....
.....
.....
.....
.....

z=2, w=2
.....
.....
..#..
.....
.....
~~~

6周期の起動プロセス全てが完了した後、
**848**の立方体が**活性状態**になっています。

指定されたあなたの初期配置から始めて、4次元空間で6周期をシミュレーションしてください。
**第6周期の後、活性状態の立方体はいくつありますか？**
