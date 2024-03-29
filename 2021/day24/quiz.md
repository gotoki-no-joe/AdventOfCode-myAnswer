# 24日目：算術論理演算装置 #

潜水艦の算術論理演算装置 (ALU) から[魔法の煙](https://en.wikipedia.org/wiki/Magic_smoke)が漏れ始めます。
（訳注：過熱した電子回路から紫煙が上がることを冗談で magic smoke と呼ぶらしい）
基本的な算術および論理演算を実行する能力がなければ、
潜水艦はクリスマスライトでクールなパターンを作り出すことができません！

おまけに、航行することも、酸素供給システムを稼働させることもできません。

でも心配はいりません。
あなたが新しいALUを構築するのに十分な時間を与えるだけの十分な酸素が残っています、**たぶん。**

ALUは4次元処理装置です。
それは整数変数 w, x, y, z を持ちます。
これらの変数はすべて値0で始まります。
ALUは**6つの命令**に対応しています。

- `inp a` 入力値を読み取り、それを変数aに書き込みます。
- `add a b` aの値にbの値を加え、結果を変数aに格納します。
- `mul a b` aの値にbの値を掛けて、結果を変数aに格納します。
- `div a b` aの値をbの値で割り、結果を整数に切り捨て、その結果を変数aに格納します。
（ここで、「切り捨て」とは、値を零の方に丸めることを意味します。）
- `mod a b` aの値をbの値で割った**余り**を変数aに格納します。（これはモジュロ演算とも呼ばれます。）
- `eql a b` aの値とbの値が等しいならば、値1を変数aに格納します。
それ以外の場合は、値0を変数aに格納します。

これらのすべての命令で、a, b はプレースホルダです。
aは常に変数（w, x, y, z のいずれか）で、そこに演算結果が格納されます。
一方で、bは変数または数値のいずれかとすることができます。
数値は正または負にすることができますが、常に整数になります。

ALUには**ジャンプ**命令がありません。
ALUプログラムでは、すべての命令が上から下に順番に1回だけ実行されます。
最後の命令の実行が終了すると、プログラムは停止します。

（プログラムの作成者は特に注意する必要があります。
b=0のときにdivを実行しようとしたり、
a<0あるいはb≦0のときにmodを実行しようとしたりすると、
プログラムがクラッシュし、ALUが損傷する可能性があります。
これらの操作はまともなALUプログラムでは試されることは決してありません。）

たとえば、次のALUプログラムは、入力の数を受け取り、それを符号反転して、xに格納します。

```
inp x
mul x -1
```

2つの入力数値をとり、ふたつめの数がひとつめの数の3倍大きければzを1にし、
そうでないときにはzを0にするALUプログラムを示します。

```
inp z
inp x
mul z 3
eql z x
```

下は、
非負の整数を入力として受け取り、それを2進数に変換し、
最下位ビット（1の位）をzに格納し、
2番目に低いビット（2の位）をyに、
3番目に低いビット（4の位）をxに、
4番目に低いビット（8の位）をwに格納するALUプログラムです。

```
inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2
```

交換用のALUを作成したら、それを潜水艦にインストールできます。
これにより、ALUが故障したときの動作がすぐに再開されます。
それは潜水艦の**モデル番号**の検証でした。
これを行うために、ALUは
モデル番号自動検出プログラム（MOdel Number Automatic Detector program, MONAD, パズル入力）を実行します。

潜水艦のモデル番号は常に、1から9までの数字のみで構成される**14桁の数**です。
モデル番号に数字0は**入りません**。

MONADが架空の14桁のモデル番号をチェックする場合、
14の個別のinp命令を使用します。
これらのinp命令はそれぞれ、最上位桁から最下位桁への順に、モデル番号の数字を**1桁**だけ読み込みます。
（つまり、モデル番号13579246899999を確認するためには、
あなたは最初のinp命令に1、2つめのinp命令に3、3つめのinp命令に5、以下同様、のように与えます。）
これは、MONADを実行する際に、
各入力命令は1以上9以下の整数値だけが与えられるということです。

次に、MONADがすべての命令の実行を終了した後、
変数zに0を残すことにより、モデル番号が**有効**であったことを示します。
モデル番号が**無効**な場合は、zに0でない値が残ります。

MONADはモデル番号に追加の不思議な制限を課しています。
伝説によれば、MONADドキュメントの最後のコピーはタヌキに食べられてしまいました。
**MONADが何をするかを**他の方法で理解する必要があります。

潜水艦の機能をできるだけ多く有効にするために、
有効な最大の14桁の数字0を含まないモデル番号を見つけてください。
**MONADが受け付ける最大のモデル番号は何ですか？**

# パート2 #

潜水艦が[Retro Encabulator](https://en.wikipedia.org/wiki/Turboencabulator)のように起動し始めると、
結局、これらの潜水艦の機能すべてが必要ではないことに気付きました。

**MONADが受け付ける最小のモデル番号は何ですか？**
