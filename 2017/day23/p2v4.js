let h = 0;
let b = 105700;
let c = b + 17000;
for (; b <= c; b = b + 17) {
    breakout:
    for (let d = 2; d < b; d++) {
        if (b % d == 0) { h++; break breakout; }
    }
}
console.log(h);

/*
>node p2v4.js
915
*/
