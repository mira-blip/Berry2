/*!tests!
 *
 * {
 *   "input":   [],
 *   "output":  ["1", "3", "5", "7", "9"]
 * }
 *
 */

void f(int xs[], int n) {
    int i = 0 ;
    while (i < n) {
        xs[i] = 2*i + 1 ;
        i = i + 1 ;
    }

    return ;
}

void main() {
    int xs[5] ;
    f(xs, 5) ;

    int i = 0 ;
    while (i < 5) {
        fprintf(stdout, "%d\n", xs[i]) ;
        i = i + 1 ;
    }

    return ;
}
