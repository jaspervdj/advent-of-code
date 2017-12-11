#include <stdio.h>

int main(int argc, char **argv) {
    int token = -1;
    int row_min = -1;
    int row_max = -1;
    int checksum = 0;
    int c;

    while(1) {
        c = fgetc(stdin);

        /* Parse (possibly part of) a token. */
        if(c >= '0' && c <= '9') {
            if(token >= 0) {
                token = token * 10 + (c - '0');
            } else {
                token = c - '0';
            }

        /* Number finished, processing. */
        } else {
            if(token >= 0) {
                if(row_min >= 0) {
                    if(token < row_min) row_min = token;
                    if(token > row_max) row_max = token;
                } else {
                    row_min = row_max = token;
                }
            }

            if(c == '\n') {
                checksum += (row_max - row_min);
                row_max = row_min = -1;
            } else if(c == EOF) {
                fprintf(stdout, "%d\n", checksum);
                return 0;
            }

            token = -1;
        }
    }
}
