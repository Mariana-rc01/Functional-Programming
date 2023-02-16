#include <stdio.h>

void triangulo (int n) {
    int i, j;
    for (i = 1; (i < 2 * n); i++) {
        for (j = 1; (j <= 2 * n - i); j++) {
            if (j <= i) putchar ('#');
        }
        putchar ('\n');
    }
}

/*
Explicaçao:
#
##
###
####
#####
####
###
##
#

Ha no total 2*n-1 linhas, sendo que na linha n tem n#'s e depois começa a descrescer ate 1 (dai o -i).
*/