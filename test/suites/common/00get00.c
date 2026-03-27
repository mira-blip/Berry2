/*!tests!
 *
 * {
 *      "input":    ["5", "true", "hello"],
 *      "output":   ["5", "true", "hello"]
 * }
 *
 * {
 *      "input":    ["-7", "false", "fred"],
 *      "output":   ["-7", "false", "fred"]
 * }
 *
 */



void main() {
    int x ;
    bool b ;
    char* s ;

    fscanf(stdin, "%d", &x) ;
    fscanf(stdin, "%b", &b) ;
    fscanf(stdin, "%s", &s) ;

    fprintf(stdout, "%d\n", x) ;
    fprintf(stdout, "%b\n", b) ;
    fprintf(stdout, "%s\n", s) ;

    return ;
}

