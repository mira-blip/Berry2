/*!tests!
 *
 *
 *  {
 *      "input":    [""],
 *      "exception":    "UnboundVariable"
 * }
 *
 */

int f(int x){
    return x*2;
}

void main() {
    {
        int x=7;
    }
    f(x);
    return;
}
