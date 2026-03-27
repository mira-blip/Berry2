/*!tests!
 *
 * {
 *   "input":   [],
 *   "output":  ["3", "7", "11", "15", "19"]
 * }
 *
 */

void f() {
    int xs[7] ;

    int i = 0 ;
    while (i < 7) {
        xs[i] = 3*i + 2 ;
        i = i + 1 ;
    }

    return ;
}

void main() {
    int xs[5] ;

    int i = 0 ;
    while (i < 5) {
        xs[i] = 4*i + 3 ;
        i = i + 1 ;
    }

    f() ;

    i = 0 ;
    while (i < 5) {
        fprintf(stdout, "%d\n", xs[i]) ;
        i = i + 1 ;
    }

    return ;

}
