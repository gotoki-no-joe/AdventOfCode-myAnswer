# 12日目：水中経路の模索 #

あなたの潜水艦の潜水航行システムは僭越ながら完全とは言えない状態にあります。
(sub-sub-sub-sub-subただの語呂合わせなので訳す必要もないけれど)
あなたができるだけ速やかにこの洞窟から抜け出すための唯一の方法は、
脱出経路を自分で見つけることです。
経路を何か**一つ**見つければよいというものではありません。
**最良の**経路を見つけたことを確信できる唯一の方法は、
**全ての**経路を見つけることです。

幸いなことに、センサーはまだほぼ機能しているので、
残りの洞窟の大まかな地図（パズル入力）を作成します。
例えば：

```
start-A
start-b
A-c
A-b
b-d
A-end
b-end
```

これは、洞窟全体がどのように繋がっているかのリストです。
`start` という名前の洞窟から出発し、目的地は `end` という名前の洞窟です。
`b-d` のような行は、洞窟 `b` が洞窟 `d` と繋がっていることを意味します。
つまり、それらの間を移動できます。

したがって、上記の洞窟はおおよそ次のようになります。

```
    start
    /   \
c--A-----b--d
    \   /
     end
```

あなたの目標は、`start` から出発し `end` で終わる、
小さな洞窟を2回以上訪れない個別の経路の数を見つけることです。
洞窟には2つの種類があります。
**大きな**洞窟（`A`のように大文字で書かれている）と
**小さな**洞窟（`b`のように小文字で書かれている）です。
小さな洞窟を何度も訪れるのは時間の無駄ですが、
大きな洞窟は十分に大きいので、何度も訪れる価値があるかもしれません。
したがって、あなたが見つけるすべての経路は、
**小さな洞窟は最大1回訪れ、大きな洞窟は何度でも訪れることができます**。

これらの規則を考えると、この例の洞窟を通過する10個の経路があります。

```
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end
```

（上記のリストの各行はひとつの経路に対応します。
その経路において訪れた洞窟は、訪れた順に列挙され、コンマで区切られています。）

この洞窟では、洞窟`d`はどの経路でも訪れることはないことに注意してください。
そうするためには、洞窟`b`を2回訪問する必要があります。
（1回は洞窟`d`に行く途中、2回目は洞窟`d`から戻るとき。）
洞窟`b`は小さいため、これは許可されていません。

少し大きい例を次に示します。

```
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
```

これを通過する19の経路は次のとおりです。

```
start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end
```

最後に、このさらに大きな例には、経路が226あります。

```
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
```

**この洞窟を通過する、小さな洞窟をたかだか１回訪れるような経路はいくつありますか？**

# パート2 #

可能な経路を確認すると、いずれか一つならば、
小さな洞窟を**2回**訪問する時間があるかもしれないことに気付きます。
具体的には、大きな洞窟は何度でも訪れることができ、
1つの小さな洞窟は最大2回、残りの小さな洞窟は最大1回訪問できます。
ただし、`start` および `end` と名前が付けられた洞窟は、**それぞれ1回だけ**訪問できます。
`start` 洞窟を離れたら、そこに戻ることはできません。
`end` 洞窟に到達したら、経路はそこで終了する必要があります。

すると、上の最初の例を通る可能な36の経路は次のとおりです。

```
start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end
```

上記の少し大きい例には103の経路があり、さらに大きい例には3509の経路があります。

これらの新しい規則を考えると、**この洞窟を通る経路はいくつありますか？**
