const test = false; // true;
const step = test ? 3 : 359;

const ub = test ? 2017 : 50000000;

const arr = new Array(ub+1);

for (let i = 1; i <= ub; i++) { arr[i] = i - 1;}
arr[0] = ub;

let t, y, x, k;

y = 4;
x = 3;
k = ub;

while (true) {
// 一つ手前y、見つけた位置xに、値kを入れる
t = arr[x];
arr[x] = -k;

if (k == 0) { break; } // 終了

arr[y] = t; // k==0のときx==yなので後にしないとおかしくなる

// tからステップ数だけ手前に戻る
x = t;
for (let i = step; i > 0; i--) {
    y = x;
    x = arr[x];
}

// k を減らす
k--;
}

if (test) {
// 2017の前後を表示
for (let i = 0; i <= 6; i++) {
    console.log(i, - +arr[i]);
}
}

// 0はxの位置に書き込んだ、その次を表示
console.log(x, +arr[x]);
const x1 = x == ub ? 0 : x + 1;
console.log(x1, - +arr[x1]);

/*
>node p2v1.js
3665345 -0
3665346 39479736
*/
