#include <stdio.h>

void triangulo (int n) {
    int i, j;
    for (i = 1; (i <= n); i++) {
        for (j = 1; (j <= n-i); j++) {
            putchar (' ');
        }
        for (j = 1; (j <= 2*i - 1); j++) {
            putchar ('#');
        }
        putchar ('\n');
    }
}

/*
Explicaçao:
    #     - 1º 1# e 4espaços
   ###    - 2º 3# e 3espaços
  #####   - 3º 5# e 2espaços
 #######  - 4º 7# e 1espaços
######### - 5º 9# e 0espaços

O nº da linha e o nº de espaços tem que dar o _n_, de modo que acrescenta espaços enquanto a soma nao for n.
De seguida, para acrescentar o nº de #, este determina-se apartir do nº da linha, para que fique
em piramide precisamos de fazer 2*nºde linhas - 1.
*/