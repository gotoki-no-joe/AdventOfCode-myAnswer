/*
キャッシュするぜ。
*/

const ws = new Array(7);
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

get1 = makeGetk(1,get0);
get2 = makeGetk(2,get1);
get3 = makeGetk(3,get2);
get4 = makeGetk(4,get3);
get5 = makeGetk(4,get4);
get6 = makeGetk(4,get5);
get7 = makeGetk(4,get6);

function get25(x) {
    return x.flatMap(get4).flatMap(get3).flatMap(get0);
}

function get75(x) {
    return x.flatMap(get7).flatMap(get3).flatMap(get0);
}

function get25x(x) {
    let y = x;
    for (let i = 1; i <= 25; i++) { y = y.flatMap(get0); }
    return y;
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

function part2() {
    const sample2 = [125,17];
    console.log(sample2.toString(), get75(sample2).length);
    const input = [510613,358,84,40702,4373582,2,0,1584]
    console.log(input.toString(), get75(input).length);
}

// part0()
// part1()
// part2()

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
