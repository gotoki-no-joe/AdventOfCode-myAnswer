# 9日目：センサーブースト #

再起動した探査車に別れを告げて火星を離れたそのとき、小惑星帯から微弱な遭難信号を受信しました。
最大の小惑星セリスにある監視ステーションからに違いありません！

信号との同調を固定するには、センサーをブーストする必要があります。
妖精は最新の**BOOST**(Basic Operation Of System Test)プログラムを送信します。

BOOST（パズル入力）はセンサーをブーストすることができますが、
不明瞭な安全上の理由から、それが実行されるコンピュータが
**完全なIntcodeコンピュータ**であることを実証するためのチェックに合格するまで、
ブースト動作を拒否します。

[既存のIntcodeコンピュータ](../day5/quiz.md)にはひとつ重要な機能がありません。
**相対モード**のオペランドへの対応が必要です。

モード2にある相対モードのオペランドは、**位置モード**のオペランドと非常によく似た動作をします。
オペランドは位置として解釈されます。
位置モードと同様に、相対モードのオペランドは読み取りまたは書き込みが可能です。

重要な違いは、相対モードのオペランドはアドレス0から数えないことです。
代わりに、**相対ベース**と呼ばれる値から数えます。
**相対ベース**の初期値は0です。

相対モードのオペランドが参照するアドレスは、それ自体と現在の**相対ベース**との**和**です。
相対ベースが0の場合、同じ値の相対モードオペランドと位置モードオペランドは同じアドレスを参照します。

例えば相対ベース`50`が与えられた場合、`-7`の相対モードオペランドはメモリアドレス`50 + -7 = 43`を参照します。

相対ベースは`相対ベースオフセット`命令で変更されます。

- オペコード`9`は、唯一のオペランドの値によって**相対ベースを調整**します。
相対ベースはオペランドの値だけ増加（または、値が負の場合は減少）します。

例えば、相対ベースが`2000`の場合、命令`109,19`の後、相対ベースは`2019`になります。
その次の命令が`204,-34`の場合、アドレス`1985`の値が出力されます。

Intcodeコンピュータには、他にもいくつかの能力が必要です。

- コンピュータの使用可能なメモリは、初期プログラムよりも大きくする必要があります。
初期プログラムを超えた位置のメモリは値`0`で始まり、
他のメモリと同様に読み書きできる必要があります。
（ただし、負のアドレスでメモリにアクセスしようとするのは不正です。）
- コンピューターは大きな数値に対応する必要があります。
BOOSTプログラムの開始近くにあるいくつかの命令はこの能力を検証します。

これらの機能を使用するプログラムの例を次に示します。

- `109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99`
入力を受け取らず、
[自分自身のコピー](https://ja.wikipedia.org/wiki/%E3%82%AF%E3%83%AF%E3%82%A4%E3%83%B3_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0))を出力します。
- `1102,34915192,34915192,7,4,7,99,0` 16桁の数値を出力します。
- `104,1125899906842624,99` 中央にある大きな数値を出力します。

BOOSTプログラムは単一の入力を要求します。
値`1`を与えることでテストモードで実行してください。
各オペコードについて一連のチェックを実行し、
正しく機能していないと思われるオペコード（および関連するアドレッシングモード）を出力し、
最後にBOOSTキーコードを出力します。

Intcodeコンピュータが完全に機能するようになると、
BOOSTプログラムはテストモードで実行したときに誤動作するオペコードがないことを報告するはずです。
するとBOOSTキーコードという単一の値のみが出力されます。
プログラムは**どんなBOOSTキーコードを生成しますか？**

# パート2 #

**これで、完全なIntcodeコンピュータが完成しました。**

ようやく、セリスの遭難信号を確認できます！
BOOSTプログラムを使用してセンサーをブーストするだけでよいです。

プログラムは、入力命令に値`2`を指定することによりセンサーブーストモードで実行されます。
実行すると、センサーを自動的にブーストしますが、
低速のハードウェアで操作を完了するには数秒かかる場合があります。
センサーブーストモードでは、プログラムは単一の値 － **遭難信号の座標** － を出力します。

BOOSTプログラムをセンサーブーストモードで実行してください。
**遭難信号の座標は何ですか？**
