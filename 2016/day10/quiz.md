# 10日目：バランスロボット #

あなたは、小さなマイクロチップをお互いに渡すことを
多くのロボットがぶんぶん(tubeへのリンク)している工場にやってきました。

さらに詳しく調べると、

ロボットはマイクロチップを**2つ**持っている場合にのみ動作し、
そうすることで、2つともを別のロボットに与えるか、
または「製品」と印された箱に入れます。
時おり、ロボットは「原料」箱からマイクロチップを取り出します。

マイクロチップの1つを分析すると、
それぞれが単一の番号を持つように見えます。
ロボットは何らかのロジックを使用して
各チップで何をすべきかを決定しているに違いありません。
あなたはローカル制御コンピュータにアクセスし、
ロボットの命令（パズル入力）をダウンロードします。

命令の中には、
特定の値のマイクロチップを特定のロボットに与えるべきことを指定するものがあります。
命令の残りの部分は、
指定されたロボットが**値の低い**または**値の高い**チップで
何をすべきかを示しています。

たとえば、次の命令を考えてみましょう。

~~~
value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2
~~~

- 最初は、ロボット1は値3のチップで始まり、
ロボット2は値2のチップと値5のチップで始まります。
- ロボット2は2つのマイクロチップを持っているので、
値の低い(2)チップをロボット1に、
値の高い(5)チップをロボット0に渡します。
- 次に、ロボット1が2つのマイクロチップを持ちます。
値2のチップを製品箱1に入れ、値3のチップをロボット0に与えます。
- 最後に、ロボット0が2つのマイクロチップを持ちます。
3を製品箱2に、5を製品箱0に入れます。

最後には、
製品箱0には値5のマイクロチップが入っており、
製品箱1には値2のマイクロチップが入り、
製品箱2には値3のマイクロチップが入ります。
この設定では、番号2のロボットが
値5のマイクロチップと値2のマイクロチップを比較する役割を担います。

あなたの命令に基づいて、
値61のマイクロチップと値17のマイクロチップを
比較する責任を負う**ロボットの番号**は何ですか？

# パート2 #

製品箱0,1,2に入ったそれぞれ1つのチップの
**値を掛け合わせる**といくつになりますか？