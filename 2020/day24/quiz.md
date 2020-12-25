# 24日目：ロビーのレイアウト #

あなたのいかだは熱帯の島に到着します。
小さなカニは優れた航海士だったことがわかりました。
あなたはリゾートホテルに向かいます。

ロビーに入ると、小さな問題があります。床が改装中です。
**新しいタイル張りの床**の設置が完了するまで、フロントに行くことすらできません。

タイルはすべて**六角形**です。
それらは、非常に特殊なカラーパターンで六角形のグリッドに配置する必要があります。
待つ気分ではなく、あなたはパターンを突き止める手伝いを申し出ます。

タイルは片側がすべて**白**で、反対側が**黒**です。
彼らは白い面を上に向けて始めます。
ロビーは、そこに映し出すべきパターンが入りきる十分な広さがあります。

修繕業者の一人が、**裏返す必要のあるタイルのリスト**をくれました（パズルの入力）。
リストの各行は、部屋の真ん中にある**基準タイル**から開始する一連の歩程を行くことによって、
反転する必要がある１枚のタイルを指定します。
（すべての行は同じ基準タイルから始まります。）

タイルは六角形であるため、すべてのタイルには、
東、南東、南西、西、北西、北東の**6つの隣接**タイルがあります。
これらの方向はあなたのリストではそれぞれ
`e`, `se`, `sw`, `w`, `nw`, `ne`と記載されています。
タイルは**区切り文字なしに**これらの方向を列挙して指定します。
たとえば`esenee`は、基準タイルから開始して、
東に1タイル、南東に1タイル、北東に1タイル、東に1タイル移動したときに
到着するタイルを指定します。

タイルは指定されるたびに、白から黒、または黒から白に裏返されます。
タイルは複数回裏返される場合があります。
たとえば、`esew`という行は基準タイルのすぐ隣のタイルを裏返し、
`nwwswee`という行は基準タイル自体を裏返します。

これはより大きな例です：

~~~
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
~~~

上記の例では、10枚のタイルが1回（黒に）裏返され、
さらに5枚が2回（黒に、次に白に）裏返されます。
これらのすべての指示に従った後、全部で**10**枚のタイルが**黒**になっていました。

修繕業者のリストに目を通し、裏返す必要のあるタイルを決定します。
すべての指示に従った後、**黒い面を上にしているタイルは何枚ですか？**

# パート2 #

ロビーのタイル張りの床は、生きた美術展示となることを目的としています。
毎日、タイルは次の規則に従ってすべて裏返されます。

- **黒**のタイルで直接隣接する黒タイルが**0または2より多い**ものはすべて、**白**に裏返します。
- **白**のタイルで**ちょうど2枚**の黒タイルと直接隣接しているものはすべて、**黒**に裏返します。

ここで、**直接隣接しているタイル**とは、
問題のタイルに直接接触している6つのタイルのことを意味します。

この規則はすべてのタイルに**同時に**適用されます。
言い換えると、まずどのタイルを裏返す必要があるかが決定され、次にすべてのタイルが同時に裏返されます。

上記の例では、指定された日数が経過した後に黒い面が上向きになっているタイルの数は次のとおりです。

~~~
Day 1: 15
Day 2: 12
Day 3: 25
Day 4: 14
Day 5: 23
Day 6: 28
Day 7: 41
Day 8: 37
Day 9: 49
Day 10: 37

Day 20: 132
Day 30: 259
Day 40: 406
Day 50: 566
Day 60: 788
Day 70: 1106
Day 80: 1373
Day 90: 1844
Day 100: 2208
~~~

この手順を合計100回実行すると、2208枚の黒いタイルが上を向くようになります。

**100日後に何枚のタイルが黒くなりますか？**
