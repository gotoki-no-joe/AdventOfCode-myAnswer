/*
キャッシュするぜ。
*/

const ws = new Array(5);
for (let i = 0; i < 7; i++) { ws[i] = new Map(); }

// 重み2^0
function get0(x) {
    if (ws[0].has(x)) { return ws[0].get(x); }
    let y;
    if (x == 0) { y = [1]; }
    else {
        const s = x.toString();
        if (s.length % 2 == 0) {
            s2 = s.length / 2;
            y = [parseInt(s.substring(0,s2)), parseInt(s.substring(s2))];
        } else {
            y = [x * 2024];
        }
    }
    ws[0].set(x, y);
    return y;
}

// 重みk
function makeGetk(i, sub) {
    return function (x) {
        if (ws[i].has(x)) { return ws[i].get(x); }
        const y = sub(x);
        const z = y.flatMap(sub);
        ws[i].set(x,z);
        return z;
    };
}

//      7654 3210
// 75 = 0100 1011
// 25 = 0001 1001

const get1 = makeGetk(1,get0);
const get2 = makeGetk(2,get1);
const get3 = makeGetk(3,get2);
const get4 = makeGetk(4,get3);

function single25(x) {
    return get4(x).flatMap(get3).flatMap(get0);
}
function get25(x) {
    return x.flatMap(single25);
}

function part1() {
    const sample2 = [125,17];
    console.log(sample2.toString(), get25(sample2).length);
    const input = [510613,358,84,40702,4373582,2,0,1584]
    console.log(input.toString(), get25(input).length);
}

function part0() {
    let x = [125, 17];
    for (let i = 1; i <= 6; i++) {
        x = x.flatMap(get0);
    }
    console.log(x.toString());
    console.log(x.length);
    x = [125, 17];
    x = x.flatMap(get2);
    x = x.flatMap(get1);
    console.log(x.toString());
    console.log(x.length);
}

const c25 = new Map();
function cache25(x) {
    if (c25.has(x)) { return c25.get(x); }
    const y = single25(x);
    c25.set(x,y);
    return y;
}

function part2() {
    const input = [510613,358,84,40702,4373582,2,0,1584];
    let acc = 0;
    for (const x of input.flatMap(cache25)) {
        for (const y of cache25(x)) {
            for (const z of cache25(y)) {
                acc += z.length;
            }
        }
    }
    return acc;
}

// part0()
// part1()
part2()

/*
ストレートにやると長すぎて大変なようだ。
25回する計算を3段やらないといけない。

25回する計算のキャッシュを作って、
1段やると217,812要素のリストになる。
2段目は同じ計算をして、
その場で、3段めとして同じ計算をするように見せて、その長さだけ足し合わせる、
ことでリストを作らずに済ませば何とかならないか。
というかだから25の75だったのかな。
*/

/*
そうだ、マトモにやるからいけないんだ。
順序に意味はないので、数xxがyy個ある、というMapにして、それでやらないと。ということだ。
なのでこの方針は放棄。
*/
