const theRow = 2978;
const theCol = 3083;

function part1(r,c) {
    const kc = r + c;
    const idx = (kc - 1) * (kc - 2) / 2 + c;

    var code = 20151125;
    for (i = 1; i < idx; i++) {
	code = code * 252533 % 33554393;
    }
    return code;
}

console.log(part1(theRow,theCol));

/*
for (var y=1; y<=6; y++) {
    for (var x=1; x<=6; x++) {
	console.log(y,x,part1(y,x));
    }
}
*/

/*
1始まりで考えるとすぐズレていかんな。

>node ver1.js
2650453
*/
