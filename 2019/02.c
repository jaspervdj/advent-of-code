#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

typedef struct {
    int *data;
    size_t used;
    size_t size;
    size_t ip;
} program;

void program_free(program *p) {
    free(p->data);
    free(p);
}

program *program_create(int size) {
    program *p = malloc(sizeof(program));
    p->size = size;
    p->data = calloc(p->size, sizeof(int));
    p->used = 0;
    p->ip = 0;
    return p;
}

void program_copy(program *dst, program *src) {
    int size = MIN(dst->size, src->size);
    memcpy(dst->data, src->data, size * sizeof(int));
    dst->used = MIN(src->used, size);
    dst->ip = src->ip;
}

void program_append(program *p, int value) {
    if(p->used >= p->size) {
        int old_size = p->size;
        p->size = old_size * 2;
        p->data = realloc(p->data, p->size * sizeof(int));
        memset(p->data + p->used, 0, old_size * sizeof(int));
    }

    p->data[p->used] = value;
    p->used++;
}

void program_print(program *p, FILE *f) {
    for(int i = 0; i < p->used; i++) {
        fprintf(f, "%d", p->data[i]);
        if(i + 1 < p->used) {
            fprintf(f, ",");
        }
    }
    fprintf(f, "\n");
}

#define OPCODE_ADD  1
#define OPCODE_MUL  2
#define OPCODE_HALT 99

int program_step(program *p) {
    int ip = p->ip, x, y, dst;
    int opcode = p->data[ip];
    switch(opcode) {
        case OPCODE_ADD:
            x = p->data[ip + 1];
            y = p->data[ip + 2];
            dst = p->data[ip + 3];
            p->data[dst] = p->data[x] + p->data[y];
            p->ip += 4;
            return 0;
        case OPCODE_MUL:
            x = p->data[ip + 1];
            y = p->data[ip + 2];
            dst = p->data[ip + 3];
            p->data[dst] = p->data[x] * p->data[y];
            p->ip += 4;
            return 0;
        case OPCODE_HALT:
            return OPCODE_HALT;
        default:
            fprintf(stderr, "unknown opcode: %d\n", opcode);
            return 1;
    }
}

int program_run(program *p) {
    int exit = program_step(p);
    while(exit == 0) {
        exit = program_step(p);
    }
    return exit;
}

int main(int argc, char **argv) {
    /* Parse program. */
    program *blueprint = program_create(4);
    int value;
    while(fscanf(stdin, "%d", &value) == 1) {
        program_append(blueprint, value);
        fscanf(stdin, ",");
    }
    program *p = program_create(blueprint->size);

    /* Part 1. */
    program_copy(p, blueprint);
    p->data[1] = 12;
    p->data[2] = 2;
    program_run(p);
    printf("%d\n", p->data[0]);

    /* Part 2. */
    int target = 19690720, noun = 0, verb = 0, output = 0;
    while(output != target) {
        program_copy(p, blueprint);
        p->data[1] = noun;
        p->data[2] = verb;
        program_run(p);
        output = p->data[0];

        if(output != target) {
            verb += 1;
            if(verb > 99) {
                verb = 0;
                noun += 1;
            }
        }
    }
    printf("%d\n", 100 * noun + verb);

    program_free(p);
    program_free(blueprint);
    return 0;
}
