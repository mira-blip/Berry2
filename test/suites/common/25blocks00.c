/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   ["1", "0"]
 * }
 *
 */



void main() {
    int x = 0 ;
    
    {
        int x = 1 ;
        fprintf(stdout, "%d\n", x) ;
    }

    fprintf(stdout, "%d\n", x) ;

    return ;

}
