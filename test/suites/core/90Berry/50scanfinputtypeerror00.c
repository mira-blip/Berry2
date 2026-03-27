/*!tests!
 *
 *
 *  {
 *      "input":    ["string"],
 *      "exception":    "TypeError"
 *}
 */

void main() {
    int x;
    fscanf(stdin, "%d", &x) ;
    fprintf(stdout, "%d", x) ;
    return;
}
