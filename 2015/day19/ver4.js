var rules = `Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg`;

var molecule = 'ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF'

function singular(str) {
    return str.replace(/(Th|Ti|Al|[a-z])/g, (s) => {
        switch (s) {
            case 'Th' : return 'T';
            case 'Ti' : return 'チ';
            case 'Al' : return 'ア';
            case 'e' : return '電';
            default : return '';
        }
    });
}
rules = singular(rules);
molecule = singular(molecule);

//rules = `H => HO
//H => OH
//O => HH`;
// molecule = "HOH";
//molecule = "HOHOHO";

var ruleArr = rules.split("\n").map(l => l.split(" => "));

// part 1
if (true) {
var set = new Set();

for (const key in ruleArr) {
    var i = 0;
    var j;
    while (0 <= (j = molecule.indexOf(ruleArr[key][0],i))) {
        set.add(molecule.substring(0,j) + ruleArr[key][1] + molecule.substring(j+ruleArr[key][0].length));
        i = j + 1;
//        console.log(ruleArr[key][0]);
    }
}

console.log(set.size);
// set.forEach(v => { console.log(v); });
}
/*
>node ver4.js
576
*/

// part 2

if (false) {
// 順方向の生成はメモリ不足になった。
/*
1 3
2 18
3 171
4 1551
5 13479
6 114365
7 959874
8 8023296
ここまで動いた。
*/
var set0 = new Set();
set0.add(ruleArr.pop()[1]);
set0.add(ruleArr.pop()[1]);
set0.add(ruleArr.pop()[1]);
// set0.forEach(v => { console.log(v); });
var count = 1;
var set = set0;

while (! set.has(molecule)) {
    console.log(count, set.size);
    var set1 = new Set();
    set.forEach(mo => {
        for (const key in ruleArr) {
            var i = 0;
            var j;
            while (0 <= (j = mo.indexOf(ruleArr[key][0],i))) {
                set1.add(mo.substring(0,j) + ruleArr[key][1] + mo.substring(j+ruleArr[key][0].length));
                i = j + 1;
            }
        }
    });
    set = set1;
    count++;
}
}

if (false) {
var set0 = new Set();
set0.add(molecule);
var count = 0;
var set = set0;

while (! set.has('電')) {
    console.log(count, set.size);
    var set1 = new Set();
    set.forEach(mo => {
        for (const key in ruleArr) {
            var i = 0;
            var j;
            while (0 <= (j = mo.indexOf(ruleArr[key][1],i))) {
                set1.add(mo.substring(0,j) + ruleArr[key][0] + mo.substring(j+ruleArr[key][1].length));
                i = j + 1;
            }
        }
    });
    set = set1;
    count++;
}

console.log(count, set.size);
// やっぱり動かない。
}
