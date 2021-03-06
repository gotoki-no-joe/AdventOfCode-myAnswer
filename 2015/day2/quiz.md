# 2日目：数学は使わない、と私は言われた #

妖精は包装紙を使い切りそうになっているので、もっと注文する必要があります。
彼らは各プレゼントの寸法（長さl、幅w、高さh）のリストを持っていて、
必要なだけを正確に注文したいです。

幸いなことに、すべてのプレゼントは箱（完全な直方体）です。
これは、それぞれの贈り物に必要な包装紙の計算を少し簡単にします。
箱の表面積 $2 \times l \times w + 2 \times w \times h + 2 \times h \times l$
を求めてください。
妖精はまた、それぞれのプレゼントに少し、
具体的には最も小さい面の面積だけ紙を余分に必要とします。

## 例 ##

- 寸法2x3x4のプレゼントは
$2*6 + 2*12 + 2*8 = 52$平方フィートに
余裕を6加えた58平方フィートの包装紙が必要です。
- 寸法1x1x10のプレゼントは
$2*1 + 2*10 + 2*10 = 42$平方フィートに
余裕を1加えた43平方フィートの包装紙が必要です。

妖精のリストに載っている数字はすべてフィートです。
注文するべき**包装紙の総面積**は何平方フィートですか？

# パート2 #

妖精はリボンも使い切りそうです。
リボンはすべて同じ幅であるため、
注文するときに気にすることは必要な長さだけで、
やはり正確に注文したいと考えています。

プレゼントを縛るために必要なリボンは、
その側面の周長のうち最短のもの、
言い換えるといずれかの面の周長の最小値です。
それぞれのプレゼントには、リボンの蝶結びも必要です。
完璧な蝶結びに必要なリボンの長さは、
プレゼントの容積の立方フィートと同じです。
彼らがどのように蝶結びを作るのか尋ねないでください。
彼らは決して教えてくれません。

## 例 ##

- 寸法2x3x4のプレゼントは、縛るために$2+2+3+3 = 10$フィートのリボンが、
蝶結びのために$2*3*4 = 24$フィートのリボンが必要で、
合わせて34フィート必要です。
- 寸法1x1x10のプレゼントは、縛るために$1+1+1+1 = 4$フィートのリボンが、
蝶結びのために$1*1*10 = 10$フィートのリボンが必要で、
合わせて14フィート必要です。

彼らが注文するべき**リボンの総長**はどれだけですか？
