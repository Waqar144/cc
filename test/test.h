void assert(int expected, int actual, const char* code);
#define ASSERT(x, y) assert(x, y, #y)
int printf();
