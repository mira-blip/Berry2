/*!tests!
 *
 * {
 *   "input":    ["false"],
 *   "exception":  "UndefinedFunction"
 * }
 *
 *  {
 *      "input":    ["true"],
 *      "output":   ["true"]
 * }
 *
 */


void main() {
    bool x ;
    fscanf(stdin, "%b", &x) ;
    if (x==false){
        f();
        return;
    }
    fprintf(stdout, "%b\n", b);
    return;
}
