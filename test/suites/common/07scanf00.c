/*!tests!
 *
 * {
 *      "input":    ["15", "true", "false", "Hello!", "Hello, world!"],
 *      "output":   [
 *          "15",
 *          "true",
 *          "false",
 *          "Hello!",
 *          "Hello,"
 *      ]
 * }
 */

/*
 *
 * Notice that reading a string terminates upon encountering any whitespace.
 * This matches the behavior of the %s conversion specifier for fscanf in C.
 */



void main() {
    int n ;
    fscanf(stdin, "%d", &n) ;
    fprintf(stdout, "%d\n", n) ;

    bool b ;
    fscanf(stdin, "%b", &b) ;
    fprintf(stdout, "%b\n", b) ;

    fscanf(stdin, "%b", &b) ;
    fprintf(stdout, "%b\n", b) ;

    char* s ;
    fscanf(stdin, "%s", &s) ;
    fprintf(stdout, "%s\n", s) ;

    fscanf(stdin, "%s", &s) ;
    fprintf(stdout, "%s\n", s) ;

    return ;

}
