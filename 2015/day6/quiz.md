# 6日目：火災の危険性あり #

あなたのご近所さんがホリデーハウスデコレーションコンテストであなたを
毎年敗北させ続けるので、
1,000x1,000グリッドに 100万本のライトを配備することに決めました。

さらに、今年は特にいい子でいたので、
理想的な照明を表示する構成の手順書をサンタからメールで受け取った。

グリッド内のライトは、各方向に0から999の番号が付けられます。
四隅のライトはそれぞれ座標`0,0`、`0,999`、`999,999`、`999,0`となる。
各指示は
`turn on`（点灯）、`turn off`（消灯）、`toggle`（反転）
のいずれかである。
同時に座標の対により境界を含む範囲が指定される。
各座標対は、長方形の対向する角を表します。
よって`0,0 through 2,2`のような座標対は
3x3の正方形の9つのライトを参照します。
すべてのライトはオフで始まります。

今年あなたのご近所さんに打ち勝つためには、
サンタから送られた手順書に従って、
あなたのライトを設定しさえすればよいのです。

例えば：

- `turn on 0,0 through 999,999` はすべてのライトをオンにする。
（元からオンのものはオンのままです）。
- `toggle 0,0 through 999,0` 第1行の1000個のライトを反転します。
オンになっていたものをオフにし、オフになっていたものをオンにします。
- `turn off 499,499 through 500,500` 中央の4つのライトをオフにします。
（元からオフのものはオフのままです。）

一通り指示書に従った後、**点灯しているライトはいくつですか？**

# パート2 #

勝利パターンを実装し終わったとき、
あなたはサンタからのメッセージを
古代ノルディック妖精語から誤って翻訳したことに気付きました。

あなたが購入したライトグリッドには、
実際には個々の輝度コントロールがありました。
各ライトは0以上の輝度を持つ。
ライトはすべてゼロから始まります。

`turn on`というフレーズは実際には
あなたがそれらのライトの明るさを1増やすべきであることを意味します。

`turn off`というフレーズは実際には
あなたがライトの明るさを1下げる必要があることを意味します。
その最小値は零です。

`toggle`のフレーズは実際には
あなたがそれらのライトの明るさを2増やすべきであることを意味します。

サンタの指示に従った後に、すべてのライトの明るさを合わせた
**総輝度**はいくつでしょう？

例えば：

- `turn on 0,0 through 0,0`は総輝度を増加させる。
- `toggle 0,0 through 999,999`は総輝度を2000000増加させる。
