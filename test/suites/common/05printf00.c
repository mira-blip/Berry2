/*!tests!
 *
 * {
 *      "input":    [],
 *      "output":   [
 *          "5",
 *          "true",
 *          "false",
 *          "Hello, world!"
 *      ]
 * }
 */



void main() {
    fprintf(stdout, "%d\n", 5) ;
    fprintf(stdout, "%b\n", true) ;
    fprintf(stdout, "%b\n", false) ;
    fprintf(stdout, "%s\n", "Hello, world!") ;

    return ;

}
