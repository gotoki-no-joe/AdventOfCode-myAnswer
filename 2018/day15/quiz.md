# 15日目：ドリンクどろぼう #

ホットチョコレートを完成させた妖精たちですが、新たな問題をかかえています。
これらの洞窟に住むゴブリンは、それを盗むために何でもします。
彼らは戦いのためにここにいるように見えます。

エリアをスキャンして、マップ（パズル入力）を生成します。
壁 (`#`)、洞窟の開いた空間 (`.`)、ゴブリン (`G`)と妖精 (`E`)のスタート位置からなります。

戦闘はラウンドで進行します。
各ラウンドで、まだ生きている各ユニットはターンを得て、次のユニットのターンが始まる前にそのすべての行動を解決します。
各ユニットはそのターンで、（まだ移動していないならば）敵の射程内に移動しようとし、（射程内に敵を収めているならば）攻撃します。

すべてのユニットは非常に訓練されており、常に非常に厳格な戦闘ルールに従います。
ユニットが斜めに移動したり攻撃したりすることは決してありません。それは不名誉な行為だからです。
複数の選択肢が同等に有効である場合、競合は上から下、次に左から右の順の読み取り順序(reading order)で解決されます。
たとえば、ユニットがラウンド内でターンを得る順序は、ユニットの種類や他のユニットがラウンドの開始後に移動したかどうかに関係なく、そのラウンドでの開始位置の読み取り順序です。例えば：

```
これらのユニットは この順にターンを得ます
  #######           #######
  #.G.E.#           #.1.2.#
  #E.G.E#           #3.4.5#
  #.G.E.#           #.6.7.#
  #######           #######
```

各ユニットは、ターンの最初にまず考えられるすべての目標（敵ユニット）を特定します。
目標が残っていない場合、戦闘は終了します。

次に、ユニットは、各目標の射程内にあるすべて空き空間 (`.`) を識別します。
それは、いずれかの目標に隣接（すぐ上、下、左、右）していて、壁や別のユニットによって占有されていないマスです。
あるいは、ユニットはすでに目標の射程内にある可能性があります。
ユニットがまだ目標の射程内になく、かつ目標の射程内にある空きマスがない場合、ユニットはそのターンを終了します。

ユニットがすでに目標の射程内にいる場合、ユニットは移動しませんが、ターン内で続けて攻撃をします。
それ以外の場合は、目標の射程内にないため、移動します。

移動するために、ユニットは最初に射程内にあるマスを考え、それらのマスのうち、どれが最も少ないステップで到達できるかを求めます。
ステップは、任意の単一の移動である隣接（すぐ上、下、左、または右）オープン（.）平方。ユニットは壁や他のユニットに移動できません。ユニットは、ユニットの現在の位置を考慮しながらこれを行い、ユニットが後でどこにあるかについての予測は行いません。ユニットが範囲内にあるマスに到達できない（へのオープンパスを見つける）ことができない場合、ユニットはそのターンを終了します。複数の正方形が範囲内にあり、結ばれている場合最短のステップで到達できるようにするために、読み取り順で最初の正方形が選択されます。例えば：

Targets:      In range:     Reachable:    Nearest:      Chosen:
#######       #######       #######       #######       #######
#E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
#...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
#.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
#######       #######       #######       #######       #######
上記のシナリオでは、エルフには3つのターゲット（3つのゴブリン）があります。

各ゴブリンには、範囲内にある隣接する開いた正方形があります（?マップ上ででマークされています）。
それらの正方形のうち、4つは到達可能です（マークされています@）。他の2つ（右側）に到達するには、壁またはユニットを移動する必要があります。
これらの到達可能な正方形のうちの3つが最も近く、2到達する（マークされた!）ために必要なステップは最も少ない（のみ）。
そのうち、読み順が最初の正方形を選択します（+）。
次に、ユニットは、選択した正方形への最短経路に沿って、その正方形に向かって1歩進みます。複数のステップでユニットが目的地に等しく近づく場合、ユニットは読み取り順に最初のステップを選択します。（これには、最短パスが複数ある場合を知って、そのような各パスの最初のステップを検討できるようにする必要があります。）例：

In range:     Nearest:      Chosen:       Distance:     Step:
#######       #######       #######       #######       #######
#.E...#       #.E...#       #.E...#       #4E212#       #..E..#
#...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
#..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
#######       #######       #######       #######       #######
エルフはターゲットの範囲内に3つの正方形を見て（?）、そのうちの2つは最も近い（!）ので、読み取り順序の最初のものが選択されます（+）。「距離」の下で、開いている各正方形は、目的の正方形からの距離でマークされています。エルフがこのターン（下と右）に移動できる2つのマス目はどちらも同じように良い動きであり、エルフの2ステップがゴブリンの範囲内にない状態になります。読み取り順で最初のステップが選択されているため、エルフは1マス右に移動します。

動きのより大きな例を次に示します。

Initially:
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########

After 1 round:
#########
#.G...G.#
#...G...#
#...E..G#
#.G.....#
#.......#
#G..G..G#
#.......#
#########

After 2 rounds:
#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########

After 3 rounds:
#########
#.......#
#..GGG..#
#..GEG..#
#G..G...#
#......G#
#.......#
#.......#
#########
ゴブリンとエルフが上の位置に到達すると、それらはすべてターゲットの範囲内にあるか、ターゲットの範囲内に正方形を見つけることができないため、ユニットが死ぬまでどのユニットも移動できません。

移動した後（またはユニットが目標の範囲内でターンを開始した場合）、ユニットは攻撃します。

攻撃するために、ユニットは最初に、そのすぐ隣にいることによって、その範囲内にあるすべてのターゲットを決定します。そのような目標がない場合、ユニットはそのターンを終了します。それ以外の場合は、ヒットポイントが最も少ない隣接ターゲットが選択されます。同点の場合、読み取り順で最初にヒットポイントが最も少ない隣接ターゲットが選択されます。

ユニットは選択したターゲットに攻撃力に等しいダメージを与え、そのヒットポイントをその量だけ減らします。これがヒットポイントを0以下に減らすと、選択されたターゲットは死にます：その正方形はになり.、それ以上のターンは必要ありません。

ゴブリンまたはエルフの各ユニットは3 攻撃力を持ち、200 ヒットポイントから始まります。

たとえば、エルフだけが攻撃しようとしているとします。

       HP:            HP:
G....  9       G....  9  
..G..  4       ..G..  4  
..EG.  2  -->  ..E..     
..G..  2       ..G..  2  
...G.  1       ...G.  1  
「HP」列は、対応する行の左側にゴブリンのヒットポイントを示しています。エルフは3つのターゲットの範囲内にあります：その上のゴブリン（4ヒットポイントあり）、その右側のゴブリン（2ヒットポイントあり）、そしてその下のゴブリン（これも2ヒットポイントあり）。3つのターゲットが範囲内にあるため、ヒットポイントが最も低いターゲットが選択されます。2ヒットポイントがそれぞれある2つのゴブリン（1つはエルフの右側、もう1つはエルフの下）です。そのうち、読み順が最初のゴブリン（エルフの右側）が選ばれます。選択したゴブリンのヒットポイント（2）はエルフの攻撃力（3）によって減少し、ヒットポイントをに減らして-1殺します。

攻撃後、ユニットのターンは終了します。ユニットのターンがどのように終了するかに関係なく、ラウンドの次のユニットが順番を取ります。このラウンドですべてのユニットが交代した場合、ラウンドは終了し、新しいラウンドが始まります。

エルフはかなり数が多いように見えます。戦闘の結果を決定する必要があります。完了したフルラウンドの数（戦闘が終了したラウンドは含まれません）に、戦闘が終了した時点で残っているすべてのユニットのヒットポイントの合計を掛けたものです。（戦闘は、ユニットがそのターン中に目標を見つけられなかった場合にのみ終了します。）

以下は戦闘全体のサンプルです。各マップの横に、各行のユニットのヒットポイントが左から右にリストされています。

Initially:
#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#   
#######   

After 1 round:
#######   
#..G..#   G(200)
#...EG#   E(197), G(197)
#.#G#G#   G(200), G(197)
#...#E#   E(197)
#.....#   
#######   

After 2 rounds:
#######   
#...G.#   G(200)
#..GEG#   G(200), E(188), G(194)
#.#.#G#   G(194)
#...#E#   E(194)
#.....#   
#######   

Combat ensues; eventually, the top Elf dies:

After 23 rounds:
#######   
#...G.#   G(200)
#..G.G#   G(200), G(131)
#.#.#G#   G(131)
#...#E#   E(131)
#.....#   
#######   

After 24 rounds:
#######   
#..G..#   G(200)
#...G.#   G(131)
#.#G#G#   G(200), G(128)
#...#E#   E(128)
#.....#   
#######   

After 25 rounds:
#######   
#.G...#   G(200)
#..G..#   G(131)
#.#.#G#   G(125)
#..G#E#   G(200), E(125)
#.....#   
#######   

After 26 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(122)
#...#E#   E(122)
#..G..#   G(200)
#######   

After 27 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(119)
#...#E#   E(119)
#...G.#   G(200)
#######   

After 28 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(116)
#...#E#   E(113)
#....G#   G(200)
#######   

More combat ensues; eventually, the bottom Elf dies:

After 47 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(59)
#...#.#   
#....G#   G(200)
#######   
第48ラウンドが終了する前に、左上のゴブリンはターゲットが残っていないことを発見したため、戦闘は終了します。したがって、完了したフルラウンドの数は47であり、残りのすべてのユニットのヒットポイントの合計はです。これらから、戦いの結果はです。200+131+59+200 = 59047 * 590 = 27730

要約された戦闘の例をいくつか示します。

#######       #######
#G..#E#       #...#E#   E(200)
#E#E.E#       #E#...#   E(197)
#G.##.#  -->  #.E##.#   E(185)
#...#E#       #E..#E#   E(200), E(200)
#...E.#       #.....#
#######       #######

Combat ends after 37 full rounds
Elves win with 982 total hit points left
Outcome: 37 * 982 = 36334
#######       #######   
#E..EG#       #.E.E.#   E(164), E(197)
#.#G.E#       #.#E..#   E(200)
#E.##E#  -->  #E.##.#   E(98)
#G..#.#       #.E.#.#   E(200)
#..E#.#       #...#.#   
#######       #######   

Combat ends after 46 full rounds
Elves win with 859 total hit points left
Outcome: 46 * 859 = 39514
#######       #######   
#E.G#.#       #G.G#.#   G(200), G(98)
#.#G..#       #.#G..#   G(200)
#G.#.G#  -->  #..#..#   
#G..#.#       #...#G#   G(95)
#...E.#       #...G.#   G(200)
#######       #######   

Combat ends after 35 full rounds
Goblins win with 793 total hit points left
Outcome: 35 * 793 = 27755
#######       #######   
#.E...#       #.....#   
#.#..G#       #.#G..#   G(200)
#.###.#  -->  #.###.#   
#E#G#G#       #.#.#.#   
#...#G#       #G.G#G#   G(98), G(38), G(200)
#######       #######   

Combat ends after 54 full rounds
Goblins win with 536 total hit points left
Outcome: 54 * 536 = 28944
#########       #########   
#G......#       #.G.....#   G(137)
#.E.#...#       #G.G#...#   G(200), G(200)
#..##..G#       #.G##...#   G(200)
#...##..#  -->  #...##..#   
#...#...#       #.G.#...#   G(200)
#.G...G.#       #.......#   
#.....G.#       #.......#   
#########       #########   

Combat ends after 20 full rounds
Goblins win with 937 total hit points left
Outcome: 20 * 937 = 18740
パズルの入力で説明されている戦闘の結果は何ですか？







To move, the unit first considers the squares that are in range and determines which of those squares it could reach in the fewest steps. A step is a single movement to any adjacent (immediately up, down, left, or right) open (.) square. Units cannot move into walls or other units. The unit does this while considering the current positions of units and does not do any prediction about where units will be later. If the unit cannot reach (find an open path to) any of the squares that are in range, it ends its turn. If multiple squares are in range and tied for being reachable in the fewest steps, the square which is first in reading order is chosen. For example:

Targets:      In range:     Reachable:    Nearest:      Chosen:
#######       #######       #######       #######       #######
#E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
#...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
#.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
#######       #######       #######       #######       #######
In the above scenario, the Elf has three targets (the three Goblins):

Each of the Goblins has open, adjacent squares which are in range (marked with a ? on the map).
Of those squares, four are reachable (marked @); the other two (on the right) would require moving through a wall or unit to reach.
Three of these reachable squares are nearest, requiring the fewest steps (only 2) to reach (marked !).
Of those, the square which is first in reading order is chosen (+).
The unit then takes a single step toward the chosen square along the shortest path to that square. If multiple steps would put the unit equally closer to its destination, the unit chooses the step which is first in reading order. (This requires knowing when there is more than one shortest path so that you can consider the first step of each such path.) For example:

In range:     Nearest:      Chosen:       Distance:     Step:
#######       #######       #######       #######       #######
#.E...#       #.E...#       #.E...#       #4E212#       #..E..#
#...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
#..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
#######       #######       #######       #######       #######
The Elf sees three squares in range of a target (?), two of which are nearest (!), and so the first in reading order is chosen (+). Under "Distance", each open square is marked with its distance from the destination square; the two squares to which the Elf could move on this turn (down and to the right) are both equally good moves and would leave the Elf 2 steps from being in range of the Goblin. Because the step which is first in reading order is chosen, the Elf moves right one square.

Here's a larger example of movement:

Initially:
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########

After 1 round:
#########
#.G...G.#
#...G...#
#...E..G#
#.G.....#
#.......#
#G..G..G#
#.......#
#########

After 2 rounds:
#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########

After 3 rounds:
#########
#.......#
#..GGG..#
#..GEG..#
#G..G...#
#......G#
#.......#
#.......#
#########
Once the Goblins and Elf reach the positions above, they all are either in range of a target or cannot find any square in range of a target, and so none of the units can move until a unit dies.

After moving (or if the unit began its turn in range of a target), the unit attacks.

To attack, the unit first determines all of the targets that are in range of it by being immediately adjacent to it. If there are no such targets, the unit ends its turn. Otherwise, the adjacent target with the fewest hit points is selected; in a tie, the adjacent target with the fewest hit points which is first in reading order is selected.

The unit deals damage equal to its attack power to the selected target, reducing its hit points by that amount. If this reduces its hit points to 0 or fewer, the selected target dies: its square becomes . and it takes no further turns.

Each unit, either Goblin or Elf, has 3 attack power and starts with 200 hit points.

For example, suppose the only Elf is about to attack:

       HP:            HP:
G....  9       G....  9  
..G..  4       ..G..  4  
..EG.  2  -->  ..E..     
..G..  2       ..G..  2  
...G.  1       ...G.  1  
The "HP" column shows the hit points of the Goblin to the left in the corresponding row. The Elf is in range of three targets: the Goblin above it (with 4 hit points), the Goblin to its right (with 2 hit points), and the Goblin below it (also with 2 hit points). Because three targets are in range, the ones with the lowest hit points are selected: the two Goblins with 2 hit points each (one to the right of the Elf and one below the Elf). Of those, the Goblin first in reading order (the one to the right of the Elf) is selected. The selected Goblin's hit points (2) are reduced by the Elf's attack power (3), reducing its hit points to -1, killing it.

After attacking, the unit's turn ends. Regardless of how the unit's turn ends, the next unit in the round takes its turn. If all units have taken turns in this round, the round ends, and a new round begins.

The Elves look quite outnumbered. You need to determine the outcome of the battle: the number of full rounds that were completed (not counting the round in which combat ends) multiplied by the sum of the hit points of all remaining units at the moment combat ends. (Combat only ends when a unit finds no targets during its turn.)

Below is an entire sample combat. Next to each map, each row's units' hit points are listed from left to right.

Initially:
#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#   
#######   

After 1 round:
#######   
#..G..#   G(200)
#...EG#   E(197), G(197)
#.#G#G#   G(200), G(197)
#...#E#   E(197)
#.....#   
#######   

After 2 rounds:
#######   
#...G.#   G(200)
#..GEG#   G(200), E(188), G(194)
#.#.#G#   G(194)
#...#E#   E(194)
#.....#   
#######   

Combat ensues; eventually, the top Elf dies:

After 23 rounds:
#######   
#...G.#   G(200)
#..G.G#   G(200), G(131)
#.#.#G#   G(131)
#...#E#   E(131)
#.....#   
#######   

After 24 rounds:
#######   
#..G..#   G(200)
#...G.#   G(131)
#.#G#G#   G(200), G(128)
#...#E#   E(128)
#.....#   
#######   

After 25 rounds:
#######   
#.G...#   G(200)
#..G..#   G(131)
#.#.#G#   G(125)
#..G#E#   G(200), E(125)
#.....#   
#######   

After 26 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(122)
#...#E#   E(122)
#..G..#   G(200)
#######   

After 27 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(119)
#...#E#   E(119)
#...G.#   G(200)
#######   

After 28 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(116)
#...#E#   E(113)
#....G#   G(200)
#######   

More combat ensues; eventually, the bottom Elf dies:

After 47 rounds:
#######   
#G....#   G(200)
#.G...#   G(131)
#.#.#G#   G(59)
#...#.#   
#....G#   G(200)
#######   
Before the 48th round can finish, the top-left Goblin finds that there are no targets remaining, and so combat ends. So, the number of full rounds that were completed is 47, and the sum of the hit points of all remaining units is 200+131+59+200 = 590. From these, the outcome of the battle is 47 * 590 = 27730.

Here are a few example summarized combats:

#######       #######
#G..#E#       #...#E#   E(200)
#E#E.E#       #E#...#   E(197)
#G.##.#  -->  #.E##.#   E(185)
#...#E#       #E..#E#   E(200), E(200)
#...E.#       #.....#
#######       #######

Combat ends after 37 full rounds
Elves win with 982 total hit points left
Outcome: 37 * 982 = 36334
#######       #######   
#E..EG#       #.E.E.#   E(164), E(197)
#.#G.E#       #.#E..#   E(200)
#E.##E#  -->  #E.##.#   E(98)
#G..#.#       #.E.#.#   E(200)
#..E#.#       #...#.#   
#######       #######   

Combat ends after 46 full rounds
Elves win with 859 total hit points left
Outcome: 46 * 859 = 39514
#######       #######   
#E.G#.#       #G.G#.#   G(200), G(98)
#.#G..#       #.#G..#   G(200)
#G.#.G#  -->  #..#..#   
#G..#.#       #...#G#   G(95)
#...E.#       #...G.#   G(200)
#######       #######   

Combat ends after 35 full rounds
Goblins win with 793 total hit points left
Outcome: 35 * 793 = 27755
#######       #######   
#.E...#       #.....#   
#.#..G#       #.#G..#   G(200)
#.###.#  -->  #.###.#   
#E#G#G#       #.#.#.#   
#...#G#       #G.G#G#   G(98), G(38), G(200)
#######       #######   

Combat ends after 54 full rounds
Goblins win with 536 total hit points left
Outcome: 54 * 536 = 28944
#########       #########   
#G......#       #.G.....#   G(137)
#.E.#...#       #G.G#...#   G(200), G(200)
#..##..G#       #.G##...#   G(200)
#...##..#  -->  #...##..#   
#...#...#       #.G.#...#   G(200)
#.G...G.#       #.......#   
#.....G.#       #.......#   
#########       #########   

Combat ends after 20 full rounds
Goblins win with 937 total hit points left
Outcome: 20 * 937 = 18740
What is the outcome of the combat described in your puzzle input?
