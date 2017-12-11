#include <stdio.h>

#define ABS(x) ((x) > 0 ? (x) : (-(x)))

int main(int argc, char **argv) {
    int needle;
    int side;
    int x, y;
    int offset;

    /* Find the side of the square. */
    fscanf(stdin, "%d", &needle);
    side = 1;
    while(side * side < needle) side += 2;

    /* Find one coordinate, we don't really care which one. */
    x = side / 2;

    /* Find the other coordinate now.  We first take the offset from the corner.
     * We can module by `(side - 1)` since we don't really care about what side
     * of the square the needle is. */
    offset = needle - (side - 2) * (side - 2);
    if(side > 1) offset %= (side - 1);
    y = ABS(offset - x);
    fprintf(stdout, "%d\n", x + y);

    return 0;
}
