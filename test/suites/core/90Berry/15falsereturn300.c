/*!tests!
 *
 * {
 *   "input":    [],
 *   "exception":   "NoReturn"
 * }
 *
 */


void main() {
    int x=1;
    while (x>10){
        x = x+1;
    }
    if (x>2) {
        fprintf(stdout, "%d\n", 1) ;
        return;
    }
    
}
