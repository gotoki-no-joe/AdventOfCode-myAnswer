// 入力データ

const sample = [3,8,9,1,2,5,4,6,7]
const input = [7,8,4,2,3,5,9,1,6]

// 10^6要素の配列を定義

const arr = new Array(1000000)

/*
配列を初期設定する
divMod 1000000 9 = (111111,1) なので、111110回は普通にカウントアップしていき、
最後の回は9を飛ばす、という芸当が必要。
じゃない、最後は1だけ入れて終わり、だ。
*/

function setArr(indata) {
    let p = 0;
    for (let base = 0; ; base += 9) {
        for (let j = 0; j < 9; j++) {
            if (p == 999999) {
                arr[p] = 1000000;
                return;
            }
            arr[p] = base + indata[j];
            p++;
        }
    }
}

setArr(sample)
// setArr(input)

/*
一回の操作 move を、current の位置から行う。
*/

function succ(x) { return x == 999999 ? 0 : x + 1; }
function pred(x) { return x == 0 ? 999999 : x - 1; }
function predcup(x) { return x == 1 ? 1000000 : x - 1; }

function move(current) {
    const v0 = arr[current];
    const c1 = succ(current);
    const v1 = arr[c1];
    const c2 = succ(c1);
    const v2 = arr[c2];
    const c3 = succ(c2);
    const v3 = arr[c3];
    const three = [v1,v2,v3];
    let dest = predcup(v0);
    while (three.includes(dest)) { dest = predcup(dest); }
// 前後を手分けして探す
//    const destPos = arr.indexOf(dest);
    let p;
    let q;
    let destPos;
    for (p = succ(c3), q = pred(current); ; p = succ(p), q = pred(q)) {
        if (arr[p] == dest) { destPos = p; break; }
        if (arr[q] == dest) { destPos = q; break; }
    }
//    if (arr.indexOf(dest) != destPos) { console.error(arr, current, destPos); }
    let i;
    let j;
    for (i = c1, j = succ(c3); j != destPos; i = succ(i), j = succ(j)) {
        arr[i] = arr[j];
    }
    arr[i] = arr[j]; // dest cup
    i = succ(i); arr[i] = v1;
    i = succ(i); arr[i] = v2;
    i = succ(i); arr[i] = v3;
}

for (let i = 0; i < 10000000; i++) {
    move(i % 1000000);
    if (i % 10000 == 0) { console.log(i); }
}

const one = arr.indexOf(1);
// const left = pred(one);
const right = arr[succ(one)];
const right2 = arr[succ(succ(one))];
console.log(right,right2,right * right2);

/*
順調に動いているけどめちゃ時間かかるな。
わかってたけど。
コンパイル言語に切り替えるかぁ。

ブン回して数時間、左と右でなくて右ふたつらしいということに気がついた。とほほ。
*/
