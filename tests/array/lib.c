#include "lib.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void f(intptr_t **a, size_t *len)
{
    if (*a == NULL)
    {
        *a = malloc(sizeof(intptr_t) * 3);
        intptr_t *b = *a;
        b[0] = 42;
        b[1] = 43;
        b[2] = 44;
        *len = 3;
    }
    else
    {
        for (size_t i = 0; i < *len; i++)
        {
            (*a)[i] += 1;
        }
    }
};
void f2(intptr_t *a, size_t len)
{
    for (size_t i = 0; i < len; i++)
    {
        printf("c:%li\n", a[i]);
        fflush(stdout);
        a[i] += 1;
    }
};
void f4(intptr_t * a) {
    a[0] = 4;
    a[1] = 5;
    a[2] = 6;
    a[3] = 7;
};