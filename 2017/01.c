#include <stdio.h>

int get_digit(FILE *stream) {
    int ret = fgetc(stream);
    if(ret >= '0' && ret <= '9') {
        return ret - '0';
    } else {
        return -1;
    }
}

int main(int argc, char **argv) {
    int first_digit;
    int current_digit;
    int next_digit;
    int captcha = 0;

    current_digit = get_digit(stdin);
    first_digit = current_digit;
    next_digit = get_digit(stdin);

    while(next_digit >= 0) {
        if(current_digit == next_digit) {
            captcha += current_digit;
        }

        current_digit = next_digit;
        next_digit = get_digit(stdin);
    }

    if(current_digit == first_digit) {
        captcha += current_digit;
    }

    fprintf(stdout, "%d\n", captcha);
    return 0;
}
