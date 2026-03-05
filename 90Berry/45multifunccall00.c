/*!tests!
 *
 *
 *  {
 *      "input":    ["5"],
 *      "output":   ["30"]
 * }
 *
 */

int f1(int x){
    return x * 3;
}


int f(int x){
    x = f1(x);
    return x * 2;
}



void main() {
    int x;
    fscanf(stdin, "%d", &x) ;
    x=f(x) ;
    fprintf(stdout, "%d", x) ;
    return;
}
