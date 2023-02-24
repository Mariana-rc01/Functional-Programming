#include <stdio.h>
#include <math.h>

/*Por ter sqrt, precisa-se de fazer o seguinte comando: gcc -o ficha1 Ficha1.c -lm 
The math library must be linked in when building the executable.
How to do this varies by environment, but in Linux/Unix, just add -lm to the command.
*/

// Exercicio 1:

void quadrado (int n) {
    int i, j;
    for (i = 0; (i < n); i++) {
       j = 0; 
       while (j < n) {
        putchar ('#');
        j++;
       }
       putchar ('\n');
    }
}

// Exercicio extra 1:

void quadrado (int n) {
    int i, j;
    for (i = 0; (i < n); i++) {
        for (j = 0; (j < n); j++) {
            if (i % 2 == 0) putchar ('_');
            else putchar ('#');
        }
        putchar('\n');
    }
}

// Exercicio 2:

void xadrez (int n) {
    int i, j;
    for (i = 0; (i < n); i++) {
       for (j = 0; (j < n); j++){
        if ((i+j) % 2 == 0) putchar ('#');
        else putchar ('_');
       }
       putchar ('\n');
    }
}

// Exercicio 3 - nos outros ficheiros

// Exercicio 4:

void circulo (int r) {
    int i, j; //posiçao que esta a ser determinada
    float dist; //distancia entre 2 pontos, o (i,j) e o raio
    int x = 0;
    for (i = 0; (i <= 2*r); i++) {
        for (j = 0; (j <= 2*r); j++) {
           dist = sqrt((i-r)*(i-r) + (j-r)*(j-r));
           if (dist <= r) putchar ('#');
           else putchar (' ');

           if (dist <= r) x++;
        }
        putchar ('\n');
    }
    printf ("O circulo tem %d #'s.\n",x);
}

/*
    #     - 0º 1# e 4espaços
  #####   - 1º 5# e 2espaços
 #######  - 2º 7# e 1espaços
 #######  - 3º 7# e 1espaços
######### - 4º 9# e 0espaços
 #######  - 5º 7# e 1espaços
 #######  - 6º 7# e 1espaços
  #####   - 7º 5# e 2espaços
    #     - 8º 1# e 4espaços

Pensar no circulo como um quadrado com lado 2*raio.
Acrescenta espaços quando o ponto nao esta dentro da distancia. Caso contrario, acrescenta #. 
*/

int main () {
    quadrado (5);
    putchar('\n');
    xadrez (5);
    putchar('\n');
    circulo (4);
    return 0;
}