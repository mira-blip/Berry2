/*!tests!
 *
 * {
 *      "input":    ["5", "1", "6", "3", "8", "4"],
 *      "output":   [
 *        "Enter array size: Enter a number: Enter a number: Enter a number: Enter a number: Enter a number: Original list:",
 *        "1", "6", "3", "8", "4",
 *        "Sorted list:",
 *        "1", "3", "4", "6", "8"
 *      ]
 * }
 *
 */



/* Pre-condition:  i > 0, xs[0:i-1] is sorted.
 * Post-condition: xs[i] is sorted.
 */
void insert(int xs[], int i) {
    /* fprintf(stdout, "insert: i = %d.\n", i) ; */
    int j = i ;

    while (j > 0 && xs[j-1] > xs[j]) {
        int t = xs[j-1] ;
        xs[j-1] = xs[j] ;
        xs[j] = t ;
        j = j - 1 ;
    }

    return ;

}

/* Pre-condition:  xs has length at least n.
 * Post-condition: xs[0:n] is sorted.
 */
void sort(int xs[], int n) {
    int i = 1 ;

    while (i < n) {
        insert(xs, i) ;
        i = i + 1 ;
    }

    return ;
}

void main() {
    int n ;

    fprintf(stdout, "Enter array size: ") ;
    fscanf(stdin, "%d", &n) ;

    int xs[n] ;

    int i = 0 ;
    while (i < n) {
        fprintf(stdout, "Enter a number: ") ;
        int m ;
        fscanf(stdin, "%d", &m) ;
        xs[i] = m ;
        i = i + 1 ;
    }

    fprintf(stdout, "%s\n", "Original list:") ;
    i = 0 ;
    while (i < n) {
        fprintf(stdout, "%d\n", xs[i]) ;
        i = i + 1 ;
    }

    sort(xs, n) ;

    fprintf(stdout, "%s\n", "Sorted list:") ;
    i = 0 ;
    while (i < n) {
        fprintf(stdout, "%d\n", xs[i]) ;
        i = i + 1 ;
    }

    return ;

}
