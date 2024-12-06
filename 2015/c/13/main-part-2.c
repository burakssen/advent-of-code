#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GUESTS 11 // Increase the maximum number of guests to include yourself
#define MAX_NAME_LENGTH 20

typedef struct
{
    char name[MAX_NAME_LENGTH];
    int happiness[MAX_GUESTS];
} Guest;

Guest guests[MAX_GUESTS];
int guest_count = 0;

int find_guest_index(const char *name)
{
    for (int i = 0; i < guest_count; i++)
    {
        if (strcmp(guests[i].name, name) == 0)
        {
            return i;
        }
    }
    return -1;
}

void add_happiness(const char *name1, const char *name2, int happiness)
{
    int index1 = find_guest_index(name1);
    int index2 = find_guest_index(name2);

    if (index1 == -1)
    {
        index1 = guest_count++;
        strcpy(guests[index1].name, name1);
        memset(guests[index1].happiness, 0, sizeof(guests[index1].happiness));
    }
    if (index2 == -1)
    {
        index2 = guest_count++;
        strcpy(guests[index2].name, name2);
        memset(guests[index2].happiness, 0, sizeof(guests[index2].happiness));
    }

    guests[index1].happiness[index2] = happiness;
}

int calculate_happiness(int *seating)
{
    int total_happiness = 0;
    for (int i = 0; i < guest_count; i++)
    {
        int left = (i == 0) ? guest_count - 1 : i - 1;
        int right = (i == guest_count - 1) ? 0 : i + 1;
        int guest = seating[i];
        total_happiness += guests[guest].happiness[seating[left]];
        total_happiness += guests[guest].happiness[seating[right]];
    }
    return total_happiness;
}

bool next_permutation(int *array, int size)
{
    int i = size - 2;
    while (i >= 0 && array[i] >= array[i + 1])
    {
        i--;
    }
    if (i < 0)
    {
        return false;
    }

    int j = size - 1;
    while (array[j] <= array[i])
    {
        j--;
    }
    int temp = array[i];
    array[i] = array[j];
    array[j] = temp;

    for (int k = i + 1, l = size - 1; k < l; k++, l--)
    {
        temp = array[k];
        array[k] = array[l];
        array[l] = temp;
    }
    return true;
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    char line[100];
    while (fgets(line, sizeof(line), file))
    {
        char name1[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH];
        char gainlose[5];
        int happiness;
        sscanf(line, "%s would %s %d happiness units by sitting next to %s.", name1, gainlose, &happiness, name2);
        name2[strlen(name2) - 1] = '\0'; // Remove the period at the end
        if (strcmp(gainlose, "lose") == 0)
        {
            happiness = -happiness;
        }
        add_happiness(name1, name2, happiness);
    }

    fclose(file);

    // Add yourself to the list with zero happiness relationships
    strcpy(guests[guest_count].name, "You");
    memset(guests[guest_count].happiness, 0, sizeof(guests[guest_count].happiness));
    for (int i = 0; i < guest_count; i++)
    {
        guests[i].happiness[guest_count] = 0;
        guests[guest_count].happiness[i] = 0;
    }
    guest_count++;

    int seating[MAX_GUESTS];
    for (int i = 0; i < guest_count; i++)
    {
        seating[i] = i;
    }

    int max_happiness = -1;
    do
    {
        int happiness = calculate_happiness(seating);
        if (happiness > max_happiness)
        {
            max_happiness = happiness;
        }
    } while (next_permutation(seating, guest_count));

    printf("Part 2: %d\n", max_happiness);
    return EXIT_SUCCESS;
}
