/*!tests!
 *
 * {
 *   "input":    [],
 *   "exception":   "NoReturn"
 * }
 *
 */

void f(){
    int x=10;
    if(x<9){
        return;
    }
}

void main() {
    f();
    return;
}
