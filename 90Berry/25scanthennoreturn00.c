/*!tests!
 *
 * {
 *   "input":    ["6"],
 *   "exception":   "NoReturn"
 * }
 *
 *  {
 *      "input":    ["12"],
 *      "output":   ["12"]
 * }
 *
 */


void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;
    if (n>10){
        fprintf(stdout, "%d", n) ;
        return;
    }
    
}
