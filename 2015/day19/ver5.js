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

molecule = singular(molecule);

const atoms = molecule.length;

const rnar = molecule.match(/A|R/g).length;
const y = molecule.match(/Y/g).length;

console.log("Number of Atoms :", atoms);
console.log("Number of RnAr :", rnar);
console.log("Number of Y :", y);

const ans2 = atoms - rnar - 2*y - 1;

console.log("Answer if exists :", ans2);

/*
>node ver5.js
Number of Atoms : 292
Number of RnAr : 72
Number of Y : 6
Answer if exists : 207
*/