/*!tests!
 *
 * {
 *  "input":    [],
 *  "output":   ["12", "14", "16"]
 * }
 *
 */



void main() {
    int x[3] ;

    x[0] = 12 ;
    x[1] = 14 ;
    x[2] = 16 ;

    fprintf(stdout, "%d\n", x[0]) ;
    fprintf(stdout, "%d\n", x[1]) ;
    fprintf(stdout, "%d\n", x[2]) ;

    return ;
}
