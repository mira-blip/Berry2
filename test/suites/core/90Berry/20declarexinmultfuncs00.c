/*!tests!
 *
 * {
 *   "input":    [],
 *      "output":   [
 *          "10",
 *          "9",
 *          "18"
 *      ]
 * }
 *
 */

void f(){
    int x=10;
    fprintf(stdout, "%d\n", x) ;
    return;
}

int f1(){
    int x=9;
    fprintf(stdout, "%d\n", x) ;
    return x*2;
}

void main() {
    f();
    int x;
    x=f1();
    fprintf(stdout, "%d\n", x) ;
    return;
}
