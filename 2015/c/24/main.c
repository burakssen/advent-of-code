#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_PACKAGES 100
#define NUM_GROUP_OPTIONS 2

long long calculate_quantum_entanglement(int *group, int size)
{
    long long qe = 1;
    for (int i = 0; i < size; i++)
    {
        qe *= group[i];
    }
    return qe;
}

int compare(const void *a, const void *b)
{
    return (*(int *)b - *(int *)a); // Sort in descending order
}

void find_optimal_configuration(int *weights, int num_weights, int target_weight, int *best_group, int *best_size, long long *best_qe)
{
    int group[MAX_PACKAGES];
    *best_size = INT_MAX;
    *best_qe = LLONG_MAX;

    // Sort weights in descending order for optimization
    qsort(weights, num_weights, sizeof(int), compare);

    // Try all possible combinations
    for (int mask = 1; mask < (1 << num_weights); mask++)
    {
        int current_sum = 0;
        int current_size = 0;
        for (int i = 0; i < num_weights; i++)
        {
            if (mask & (1 << i))
            {
                current_sum += weights[i];
                group[current_size++] = weights[i];
            }
        }
        if (current_sum == target_weight)
        {
            if (current_size < *best_size ||
                (current_size == *best_size && calculate_quantum_entanglement(group, current_size) < *best_qe))
            {
                *best_size = current_size;
                *best_qe = calculate_quantum_entanglement(group, current_size);
                memcpy(best_group, group, current_size * sizeof(int));
            }
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];
    int group_options[NUM_GROUP_OPTIONS] = {3, 4}; // Define the array of group numbers

    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        perror("Error opening file");
        exit(1);
    }

    int weights[MAX_PACKAGES];
    int num_weights = 0;
    int total_weight = 0;
    while (fscanf(file, "%d", &weights[num_weights]) == 1 && num_weights < MAX_PACKAGES)
    {
        total_weight += weights[num_weights];
        num_weights++;
    }
    fclose(file);

    for (int i = 0; i < NUM_GROUP_OPTIONS; i++)
    {
        int num_groups = group_options[i];
        if (total_weight % num_groups != 0)
        {
            printf("Total weight is not divisible by %d. Skipping this configuration.\n", num_groups);
            continue;
        }

        int target_weight = total_weight / num_groups;
        int best_group[MAX_PACKAGES];
        int best_size;
        long long best_qe;

        find_optimal_configuration(weights, num_weights, target_weight, best_group, &best_size, &best_qe);

        if (best_size == INT_MAX)
        {
            printf("No valid configuration found for %d groups.\n", num_groups);
        }
        else
        {

            printf("Part %d: %lld\n", i + 1, best_qe);
        }
    }

    return 0;
}
