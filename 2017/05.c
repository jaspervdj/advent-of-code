#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int scan_ints(FILE *stream, int **ints) {
    size_t size = 32;
    int ints_len = 0;
    int res;

    *ints = calloc(size, sizeof(int));

    res = fscanf(stream, "%d", *ints);
    while(res == 1) {
        ints_len++;
        if(ints_len >= size) {
            size *= 2;
            *ints = realloc(*ints, size * sizeof(int));
        }

        res = fscanf(stream, "%d", *ints + ints_len);
    }

    return ints_len;
}

int solve(int weird_jumps, int *jumps, int jumps_len) {
    int idx = 0;
    int steps = 0;
    int offset;

    while(idx >= 0 && idx < jumps_len) {
        offset = jumps[idx];
        if(weird_jumps && offset >= 3) jumps[idx]--;
        else jumps[idx]++;
        idx += offset;
        steps++;
    }

    return steps;
}

int main(int argc, char **argv) {
    int *jumps1;
    int *jumps2;
    int jumps_len;

    jumps_len = scan_ints(stdin, &jumps1);
    jumps2 = malloc(jumps_len * sizeof(int));
    memcpy(jumps2, jumps1, jumps_len * sizeof(int));

    fprintf(stdout, "Steps: %d\n", solve(0, jumps1, jumps_len));
    fprintf(stdout, "Steps (weird jumps): %d\n", solve(1, jumps2, jumps_len));

    free(jumps1);
    free(jumps2);
    return 0;
}
