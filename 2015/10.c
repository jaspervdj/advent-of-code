#include <stdio.h>
#include <stdlib.h>

struct int_list {
    int value;
    struct int_list *next;
};

void int_list_free(struct int_list *list) {
    struct int_list *next;
    while(list) {
        next = list->next;
        free(list);
        list = next;
    }
}

struct int_list *int_list_from_string(char *string) {
    struct int_list *head = malloc(sizeof(struct int_list));
    struct int_list *cursor = head;
    while(cursor) {
        cursor->value = *string - '0';
        string++;
        if(*string) {
            cursor->next = malloc(sizeof(struct int_list));
        } else {
            cursor->next = 0;
        }
        cursor = cursor->next;
    }
    return head;
}

struct int_list *int_list_append(struct int_list *list, int value) {
    if(list) {
        while(list->next) {
            list = list->next;
        }
        list->next = malloc(sizeof(struct int_list));
        list->next->value = value;
        list->next->next = 0;
        return list->next;
    } else {
        list = malloc(sizeof(struct int_list));
        list->value = value;
        list->next = 0;
        return list;
    }
}

struct int_list *int_list_look_say(struct int_list *list) {
    struct int_list *prev = 0;
    struct int_list *head = 0;
    while(list) {
        int value, count;

        value = list->value;
        count = 1;

        list = list->next;
        while(list && list->value == value) {
            count++;
            list = list->next;
        }

        prev = int_list_append(prev, count);
        if(!head) head = prev;
        prev = int_list_append(prev, value);
    }
    return head;
}

int int_list_length(struct int_list *list) {
    int length = 0;
    while(list) {
        length++;
        list = list->next;
    }
    return length;
}

void int_list_print(struct int_list *list, FILE *f) {
    while(list) {
        fprintf(f, "%d", list->value);
        list = list->next;
    }
    fprintf(f, "\n");
}

int main(int argc, char **argv) {
    char string[100];

    fscanf(stdin, "%s", string);
    struct int_list *il = int_list_from_string(string);

    for(int i = 0; i < 40; i++) {
        struct int_list *next = int_list_look_say(il);
        int_list_free(il);
        il = next;
    }
    printf("%d\n", int_list_length(il));

    for(int i = 0; i < 10; i++) {
        struct int_list *next = int_list_look_say(il);
        int_list_free(il);
        il = next;
    }
    printf("%d\n", int_list_length(il));

    int_list_free(il);
    return 0;
}
