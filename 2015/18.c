#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char *lights;
    int width;
    int height;
} grid;

grid *grid_create(int width, int height) {
    grid *g = malloc(sizeof(grid));
    g->lights = calloc(width * height, sizeof(char));
    g->width = width;
    g->height = height;
    return g;
}

void grid_free(grid *g) {
    free(g->lights);
    free(g);
}

void grid_copy(grid *dst, grid *src) {
    memcpy(dst->lights, src->lights, src->width * src->height * sizeof(char));
}

void grid_read(grid *g, FILE *f) {
    /* `+ 2` on the next line since we need space for newline character and nul
     * byte at the end. */
    char *buffer = calloc(g->width + 2, sizeof(char));
    for(int y = 0; y < g->height; y++) {
        fgets(buffer, g->width + 2, f);
        for(int x = 0; x < g->width; x++) {
            g->lights[y * g->width + x] = (buffer[x] == '#') ? 1 : 0;
        }
    }
    free(buffer);
}

void grid_write(grid *g, FILE *f) {
    char *buffer = calloc(g->width + 1, sizeof(char));
    for(int y = 0; y < g->height; y++) {
        for(int x = 0; x < g->width; x++) {
            buffer[x] = g->lights[y * g->width + x] ? '#' : '.';
        }
        fprintf(f, "%s\n", buffer);
    }
    free(buffer);
}

void grid_step(grid *dst, grid *src) {
    for(int y = 0; y < src->height; y++) {
        for(int x = 0; x < src->width; x++) {
            int src_on = src->lights[y * src->width + x];
            int neighbors_on = 0;
            int dst_on;
            for(int ny = y - 1; ny <= y + 1; ny++) {
                for(int nx = x - 1; nx <= x + 1; nx++) {
                    if(ny >= 0 && ny < src->height &&
                            nx >= 0 && nx < src->width &&
                            (nx != x || ny != y)) {
                        if(src->lights[ny * src->width + nx]) {
                            neighbors_on++;
                        }
                    }
                }
            }
            if(src_on) {
                dst_on = (neighbors_on == 2 || neighbors_on == 3);
            } else {
                dst_on = (neighbors_on == 3);
            }
            dst->lights[y * src->width + x] = dst_on;
        }
    }
}

void grid_turn_on_corners(grid *g) {
    g->lights[0               * g->width + 0           ] = 1;
    g->lights[(g->height - 1) * g->width + 0           ] = 1;
    g->lights[0               * g->width + g->width - 1] = 1;
    g->lights[(g->height - 1) * g->width + g->width - 1] = 1;
}

int grid_count(grid *g) {
    int lights = 0;
    for(int y = 0; y < g->height; y++) {
        for(int x = 0; x < g->width; x++) {
            if(g->lights[y * g->width + x]) {
                lights++;
            }
        }
    }
    return lights;
}

int main(int argc, char **argv) {
    int grid_size = 100;
    int num_steps = 100;
    grid *original = grid_create(grid_size, grid_size);
    grid *src = grid_create(grid_size, grid_size);
    grid *dst = grid_create(grid_size, grid_size);

    grid_read(original, stdin);

    grid_copy(src, original);
    for(int i = 0; i < num_steps; i++) {
        grid_step(dst, src);
        if(i + 1 < num_steps) {
            grid *tmp = src;
            src = dst;
            dst = tmp;
        }
    }
    fprintf(stdout, "%d\n", grid_count(dst));

    grid_copy(src, original);
    grid_turn_on_corners(src);
    for(int i = 0; i < num_steps; i++) {
        grid_step(dst, src);
        grid_turn_on_corners(dst);
        if(i + 1 < num_steps) {
            grid *tmp = src;
            src = dst;
            dst = tmp;
        }
    }
    fprintf(stdout, "%d\n", grid_count(dst));

    grid_free(src);
    grid_free(dst);
    grid_free(original);
    return 0;
}
