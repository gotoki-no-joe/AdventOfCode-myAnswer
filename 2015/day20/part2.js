/*
配列に上限のないJSだから、undefinedにだけ気をつけて何とかする。
*/

const theinput = 36000000;

var ar = new Array();

function add(i,k) {
  if (ar[i]) { ar[i] += k; } else { ar[i] = k; }
}

/*
走りながら探す
k番目のelfに仕事をさせる。
すると、ar[k]が確定するので、判定する。
だめなら次に進む。
を繰り返す。
*/

var k = 1;

while (1) {
  const k11 = k * 11;
  for (var i=1; i<=50; i++) { add(i*k,k11); }
  if (k < 10) { console.log(k, ar[k]); }
  if (ar[k] >= theinput) { console.log(k, ar[k]); break; }
  k++;
}

/*
>node part2.js
1 11
2 33
3 44
4 77
5 66
6 132
7 88
8 165
9 143
884520 36191925
*/