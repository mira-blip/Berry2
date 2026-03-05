/*!tests!
 *
 *
 *  {
 *      "input":    [""],
 *      "exception":    "TypeError"
 * }
 *
 */

int f(int x){
    return x*2;
}

void main() {
    int x=2;
    char* s="string";
    int y = f(s);
    fprintf(stdout, "%d\n", y) ;
    return;
}
