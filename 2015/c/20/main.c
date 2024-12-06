#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Function to calculate the number of presents delivered to a house for Part One
int calculate_presents_part1(int house)
{
    int presents = 0;
    int sqrt_house = (int)sqrt(house);

    for (int elf = 1; elf <= sqrt_house; elf++)
    {
        if (house % elf == 0)
        {
            presents += elf * 10;
            if (elf != house / elf)
            {
                presents += (house / elf) * 10;
            }
        }
    }
    return presents;
}

// Function to calculate the number of presents delivered to a house for Part Two
int calculate_presents_part2(int house)
{
    int presents = 0;
    int sqrt_house = (int)sqrt(house);

    for (int elf = 1; elf <= sqrt_house; elf++)
    {
        if (house % elf == 0)
        {
            if (house / elf <= 50)
            {
                presents += elf * 11;
            }
            if (elf <= 50 && elf != house / elf)
            {
                presents += (house / elf) * 11;
            }
        }
    }
    return presents;
}

int main(int argc, char *argv[])
{
    // Ensure a file name is provided as a command line argument
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    // Open the input file
    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    // Read the number of presents from the file
    int target_presents;
    if (fscanf(file, "%d", &target_presents) != 1)
    {
        fprintf(stderr, "Error reading target presents\n");
        fclose(file);
        return 1;
    }
    fclose(file);

    // Part 1
    int house_part1 = 1;
    while (1)
    {
        int presents = calculate_presents_part1(house_part1);
        if (presents >= target_presents)
        {
            break;
        }
        house_part1++;
    }
    printf("Part 1: %d\n", house_part1);

    // Part 2
    int house_part2 = 1;
    while (1)
    {
        int presents = calculate_presents_part2(house_part2);
        if (presents >= target_presents)
        {
            break;
        }
        house_part2++;
    }
    printf("Part 2: %d\n", house_part2);

    return 0;
}
