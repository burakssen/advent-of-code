#include <stdio.h>
#include <stdlib.h>

// Function to count combinations that sum up to the target with a specific container count
void count_combinations(int containers[], int size, int target, int index, int current_count, int *min_containers, int *ways_with_min_containers)
{
    // Base case: if the target is 0, we found a valid combination
    if (target == 0)
    {
        if (current_count < *min_containers)
        {
            *min_containers = current_count;
            *ways_with_min_containers = 1;
        }
        else if (current_count == *min_containers)
        {
            (*ways_with_min_containers)++;
        }
        return;
    }
    // Base case: if the target becomes negative or no more containers to process
    if (target < 0 || index == size)
    {
        return;
    }
    // Recur by including the current container
    count_combinations(containers, size, target - containers[index], index + 1, current_count + 1, min_containers, ways_with_min_containers);
    // Recur by excluding the current container
    count_combinations(containers, size, target, index + 1, current_count, min_containers, ways_with_min_containers);
}

// Function to count total combinations that sum up to the target
int total_combinations(int containers[], int size, int target, int index)
{
    if (target == 0)
    {
        return 1;
    }
    if (target < 0 || index == size)
    {
        return 0;
    }
    int include_current = total_combinations(containers, size, target - containers[index], index + 1);
    int exclude_current = total_combinations(containers, size, target, index + 1);
    return include_current + exclude_current;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    int containers[100];
    int size = 0;
    while (fscanf(file, "%d", &containers[size]) != EOF)
    {
        size++;
    }

    fclose(file);

    int target = 150;
    int total_ways = total_combinations(containers, size, target, 0);
    printf("Part 1: %d\n", total_ways);

    int min_containers = size + 1; // initialize to a value larger than any possible number of containers
    int ways_with_min_containers = 0;

    count_combinations(containers, size, target, 0, 0, &min_containers, &ways_with_min_containers);

    printf("Part 2: %d\n", ways_with_min_containers);

    return 0;
}
