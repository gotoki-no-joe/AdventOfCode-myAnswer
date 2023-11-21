#include <stdio.h>

int main() {
    int r0 = 1, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0;
    int cont = 1;
    while (cont) {
        switch (r3) {
            case  0 : r3 = 16;              break; // addi 3 16 3 ->17]
            case  1 : r5 = 1;                r3++; // seti 1 9 5 [26->, [35->
            case  2 : r4 = 1;                r3++; // seti 1 1 4 [15->
            case  3 : r2 = r5 * r4;          r3++; // mulr 5 4 2 [11->
            case  4 : r2 = r2 == r1 ? 1 : 0; r3++; // eqrr 2 1 2
            case  5 : r3 = r2 + 5;          break; // addr 2 3 3 r5*r4==r1なら7へ、さもなくば6へ(乱入がなければ)
            case  6 : r3 = 7;               break; // addi 3 1 3 ->8]                        8
            case  7 : r0 += r5;              r3++; // addr 5 0 0
            case  8 : r4 += 1;               r3++; // addi 4 1 4 [6->
            case  9 : r2 = r4 > r1 ? 1 : 0;  r3++; // gtrr 4 1 2
            case 10 : r3 += r2;             break; // addr 3 2 3 r4>r1なら12へ、さもなくば11へ(乱入がなければ)
            case 11 : r3 = 2;               break; // seti 2 3 3 ->3]                      3
            case 12 : r5 +=1;                r3++; // addi 5 1 5
            case 13 : r2 = r5 > r1 ? 1 : 0;  r3++; // gtrr 5 1 2
            case 14 : r3 += r2;             break; // addr 2 3 3 r5>r1なら16へ、さもなくば15へ(乱入がなければ)
            case 15 : r3 = 1;               break; // seti 1 4 3 ->2]
            case 16 : r3 = 256;             break; // mulr 3 3 3 [0-> ok = halt
            case 17 : r1 += 2;               r3++; // addi 1 2 1
            case 18 : r1 *= r1;              r3++; // mulr 1 1 1
            case 19 : r1 *= 19;              r3++; // mulr 3 1 1
            case 20 : r1 *= 11;              r3++; // muli 1 11 1
            case 21 : r2 += 2;               r3++; // addi 2 2 2
            case 22 : r2 *= 22;              r3++; // mulr 2 3 2
            case 23 : r2 += 20;              r3++; // addi 2 20 2
            case 24 : r1 += r2;              r3++; // addr 1 2 1
            case 25 : r3 += r0;             break; // addr 3 0 3 ?? r0=0のパート1では26->1へ、r0=1のパート2では27へ、の分岐。
            case 26 : r3 = 0;               break; // seti 0 4 3 ->1]
            case 27 : r2 = 27;               r3++; // setr 3 9 2
            case 28 : r2 *= 28;              r3++; // mulr 2 3 2
            case 29 : r2 += 29;              r3++; // addr 3 2 2
            case 30 : r2 *= 30;              r3++; // mulr 3 2 2
            case 31 : r2 *= 14;              r3++; // muli 2 14 2
            case 32 : r2 *= 32;              r3++; // mulr 2 3 2
            case 33 : r1 += r2;              r3++; // addr 1 2 1
            case 34 : r0 = 0;                r3++; // seti 0 6 0
            case 35 : r3 = 0;               break; // seti 0 0 3 ->1]
            default : cont = 0;
        }
        r3++; // printf("%d\n", r3);
    }
    printf("%d\n", r0);
    return 0;

}
