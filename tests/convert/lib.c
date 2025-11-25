#include "lib.h"

static int trigger = 0;

int f(int i, int *data)
{
    *data = i;
    trigger++;
    return (trigger % 2? i: 0);
}