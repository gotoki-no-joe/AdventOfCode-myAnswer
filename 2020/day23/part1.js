// コードの検証のために、まずは Part1 をこちらでも再現しておく。

// 入力データ

const sample = [3,8,9,1,2,5,4,6,7]
const input = [7,8,4,2,3,5,9,1,6]

const arr = new Array(9)

function setArr(indata) {
    let p = 0;
    for (let j = 0; j < 9; j++) {
        arr[p] = indata[j];
        p++;
    }
}

setArr(sample)
// setArr(input)

/*
一回の操作 move を、current の位置から行う。
*/

function succ(x) { return x == 8 ? 0 : x + 1; }
function pred(x) { return x == 1 ? 9 : x - 1; }

function move(current) {
    console.log(arr);
    const v0 = arr[current];
    const c1 = succ(current);
    const v1 = arr[c1];
    const c2 = succ(c1);
    const v2 = arr[c2];
    const c3 = succ(c2);
    const v3 = arr[c3];
    const three = [v1,v2,v3];
    let dest = pred(v0);
    while (three.includes(dest)) { dest = pred(dest); }
    const destPos = arr.indexOf(dest);
    console.log(dest, destPos);
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

for(let i = 0; i < 100; i++) { move(i % 9); }

console.log(arr);
