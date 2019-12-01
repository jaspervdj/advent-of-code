#include <stdio.h>

int fuel_1(int mass) {
    return mass / 3 - 2;
}

int fuel_2(int mass) {
    int total_fuel = 0;
    int fuel = fuel_1(mass);
    while(fuel > 0) {
        total_fuel += fuel;
        fuel = fuel_1(fuel);
    }
    return total_fuel;
}

int main(int argc, char **argv) {
    int mass;
    int total_fuel_1 = 0;
    int total_fuel_2 = 0;
    while(fscanf(stdin, "%d", &mass) == 1) {
        total_fuel_1 += fuel_1(mass);
        total_fuel_2 += fuel_2(mass);
    }
    printf("%d\n", total_fuel_1);
    printf("%d\n", total_fuel_2);
    return 0;
}
