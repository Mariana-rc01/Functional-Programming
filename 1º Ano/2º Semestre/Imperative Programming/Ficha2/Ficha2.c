#include <stdio.h>

// Exercicio 1:

float multInt1 (int n, float m) {
    float r = 0;
    for (int i = 0; i < n; i++) {
        r += m;
    }
    return r;
}

// Exercicio 2:

float multInt2 (int n, float m) {
    float r = 0;
    while (n != 1) {
        if (n % 2 == 1) r += m;
        n /= 2;
        m *= 2;
    }
    return r;
}

// Conta quantas operaçoes (entre floats) sao feitas:

float multInt2M (int n, float m) {
    float r = 0;
    int i = 0;
    while (n != 1) {
        if (n % 2 == 1) r += m;
        n /= 2;
        m *= 2;
        i++;
    }
    return r,i;
}

// Exercicio 3:
// O maior vai ser sempre o a e o menor o b

int mdc1 (int a, int b) {
    int d;
    if (a < b) {
        d = a;
        a = b;
        b = d;
    }
    for (int i = 1; i <= b; i++) {
        if (a % i == 0 && b % i == 0) d = i;
    }
    return d;
}

// Exercicio 4:

int mdc2 (int a, int b) {
    while ((a != 0 && b != 0)) {
        if (a > b) a -= b;
        else if (a < b) b -= a;
        else return a;
        // quer dizer que os numeros sao iguais e o mdc (a,b) = a ou b
    }
    if (a == 0) return b;
    else return a;
}

// Conta quantas iteraçoes foram feitas:

int mdc2M (int a, int b) {
    int i = 0;
    while ((a != 0 && b != 0)) {
        if (a > b) a -= b;
        else if (a < b) b -= a;
        else return a;
        i++;
        // quer dizer que os numeros sao iguais e o mdc (a,b) = a ou b
    }
    if (a == 0) return b,i;
    else return a,i;
}

// Exercicio 5:

int mdc3 (int a, int b) {
    int i = 0;
    while ((a != 0 && b != 0)) {
        if (a > b) a %= b;
        else if (a < b) b %= a;
        else return a;
        i++;
        // quer dizer que os numeros sao iguais e o mdc (a,b) = a ou b
    }
    if (a == 0) return b,i;
    else return a,i;
}

// Exercicio 6:

// a)

int fib1 (int n) {
    if (n == 1 || n == 2) return 1;
    else return fib1 (n-1) + fib1 (n-2);
}

// b)

int fib2 (int n) {
    int a = 1, b = 1;
    int aux;
    for (int i = 3; i <= n; i++) {
        aux = a;
        a += b;
        b = aux;
    }
    return a;
}