# 6日目：信号とノイズ #

サンタとの通信が何者かに妨害されています。
幸いにも、あなたの信号は部分的に妨害されているだけで、
このような状況でのプロトコルは
メッセージをやりとりできるように
**単純反復コード**に切り替えることです。

このモデルでは、同じメッセージが繰り返し送信されます。
あなたは繰り返すメッセージ信号を記録しました（あなたのパズル入力）。
しかしそのデータはかなり壊れているように見えます。**ほぼ。**

あなたがする必要があることは、
各位置で最も頻繁な文字はどれかを知ることです。
たとえば、次のメッセージを記録したとします。

~~~
eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
~~~

第1列の最も頻出した文字は`e`です。
第2列は`a`、第3列は`s`、以下同様です。
これらの文字を組み合わせると、
エラーが修正されたメッセージ`easter`が得られます。

パズル入力にある記録について、
送信された**メッセージのエラー修正版は何ですか？**

# パート2 #

もちろん、その導き出した答えが確かにメッセージ**だった**のだろう。
**修正反復コード**の使用に同意しなかった場合は。

この修正コードでは、送信者は代わりにランダムなデータのように見えるものを送信しますが、
しかし各文字について、実際に送信したい文字は他の文字よりも**若干確率が低く**なります。
信号妨害ノイズが発生した後でも、
各列の文字分布を見て、
**最も頻度の低い文字**を選択することで
元のメッセージを再構成することができます。

上の例では、
第1列の最も頻度の低い文字は`a`、
第2列は`d`、などとなります。
残りの文字についてこの処理を繰り返すと、
元のメッセージ`advent`が生成されます。

あなたのパズルの入力とこの新しいデコード方式によると、
サンタが送信しようとした**元のメッセージは何ですか？**