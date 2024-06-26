#include <stdio.h>
#include <stdlib.h>

void assert(int expected, int actual, const char* code)
{
    if (expected == actual) {
        printf("%s => %d\n", code, actual);
    } else {
        printf("%s => %d expected but got %d\n", code, expected, actual);
        exit(1);
    }
}

static int static_fn() { return 5; }
