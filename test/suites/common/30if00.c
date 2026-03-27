/*!tests!
 *
 * {
 *      "input":    ["true"],
 *      "output":   ["0", "2"]
 * }
 *
 * {
 *      "input":    ["false"],
 *      "output":   ["1", "3"]
 * }
 *
 */



void main() {
    bool b ;
    fscanf(stdin, "%b", &b) ;

    if (b) fprintf(stdout, "%s\n", "0") ;
    else fprintf(stdout, "%s\n", "1") ;

    if (b) fprintf(stdout, "%s\n", "2") ;

    if (!b) fprintf(stdout, "%s\n", "3") ;

    return ;
}
