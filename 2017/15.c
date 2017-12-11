#include <stdio.h>

inline long long int a_next(const long long int a_prev) {
    return (a_prev * 16807) % 2147483647;
}

inline long long int b_next(const long long int b_prev) {
    return (b_prev * 48271) % 2147483647;
}

inline long long int a_next_3(const long long int a_prev) {
    long long int a = a_next(a_prev);
    while((a & 0x3) != 0) a = a_next(a);
    return a;
}

inline long long int b_next_4(const long long int b_prev) {
    long long int b = b_next(b_prev);
    while((b & 0x7) != 0) b = b_next(b);
    return b;
}

inline int match16(const long long int a, const long long int b) {
    return (a & 0xFFFF) == (b & 0xFFFF);
}

int problem01(const long long int a_seed, const long long int b_seed) {
    int matches = 0;
    long long int a = a_seed;
    long long int b = b_seed;
    int i;

    for(i = 0; i < 40000000; i++) {
        a = a_next(a);
        b = b_next(b);
        matches += match16(a, b);
    }

    return matches;
}

int problem02(const long long int a_seed, const long long int b_seed) {
    int matches = 0;
    long long int a = a_seed;
    long long int b = b_seed;
    int i;

    for(i = 0; i < 5000000; i++) {
        a = a_next_3(a);
        b = b_next_4(b);
        matches += match16(a, b);
    }

    return matches;
}

int main(int argc, char **argv) {
    long long int a_seed;
    long long int b_seed;
    char ignored;

    fscanf(stdin, "Generator %c starts with %lld\n", &ignored, &a_seed);
    fscanf(stdin, "Generator %c starts with %lld\n", &ignored, &b_seed);

    fprintf(stdout, "Matches (1): %d\n", problem01(a_seed, b_seed));
    fprintf(stdout, "Matches (2): %d\n", problem02(a_seed, b_seed));

    return 0;
}
