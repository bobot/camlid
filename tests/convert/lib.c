#include "lib.h"

static int trigger = 0;

intptr_t f(intptr_t i, intptr_t *data)
{
    *data = i;
    trigger++;
    return (trigger % 2? i: 0);
}
