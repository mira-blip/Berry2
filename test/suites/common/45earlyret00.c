/*!tests!
 *
 * {
 *    "input":      [],
 *    "output":     ["true"]
 * }
 *
 */



bool f() {
    if (true) return true ;

    return false ;
}

void main() {
    fprintf(stdout, "%b\n", f()) ;

    return ;
}
