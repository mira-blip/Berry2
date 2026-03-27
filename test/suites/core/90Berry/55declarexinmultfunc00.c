/*!tests!
 *
 *
 *  {
 *      "input":    [],
 *      "output": ["7"]
 *}
 */


void f1(){
    int x=7;
    fprintf(stdout, "%d", x) ;
    return;
}

void f(){
    int x=1;
    {
        int x=3;
        f1();
    }
    return;
}

void main() {
    int x=2;
    f1();
    return;
}
