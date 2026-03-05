/*!tests!
 *
 *
 *  {
 *      "input":    ["2"],
 *      "output":   ["8", "4"]
 * }
 *
 */

int f(int x){
    return x * 2;
}



void main() {
    int x;
    fscanf(stdin, "%d", &x) ;
    x=f(x) ;
    {
        int y=x;
        int x;
        x=f(y);
        fprintf(stdout, "%d", x) ;
    }
    fprintf(stdout, "%d", x) ;
}
