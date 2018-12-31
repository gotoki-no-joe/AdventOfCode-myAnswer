/*
k番目の家は、それが素数であった最悪の場合でも
1とkの妖精は来るので、k+1は貰える。これが最小。
ということで、それ以下の番号で目的の家は見つかるから
先行で計算しても大丈夫。
*/

const theinput = 36000000
const theinput10 = theinput / 10

var ar = new Array(theinput10+1);

// Elf#1
ar.fill(1);

for (var k=2;k<=theinput10; k++) {
    for (var i=k; i<=theinput10; i+=k) {
        ar[i] += k;
    }
}

console.log("Show first 9 elems.");
for (var i=1; i<=9; i++) { console.log(ar[i]); }

console.log("part1 answer");
for (var i=1; i<=theinput10; i++) {
    if (ar[i] >= theinput10) { console.log(i, ar[i]); break; }
}

/*
>node part1.js
Show first 9 elems.
1
3
4
7
6
12
8
15
13
part1 answer
831600 3690240
*/