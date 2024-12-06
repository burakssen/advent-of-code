#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOCATIONS 20
#define INF 1000000

// Function prototypes
void parse_input(FILE *file, int distances[MAX_LOCATIONS][MAX_LOCATIONS], int *num_locations);
int calculate_route_distance(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int route[], int num_locations);
int find_shortest_route(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int num_locations);
int find_longest_route(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int num_locations);
int next_permutation(int *array, int size);

// Helper function to swap elements
void swap(int *x, int *y)
{
    if (x != y)
    {
        *x ^= *y;
        *y ^= *x;
        *x ^= *y;
    }
}

// Function to compute the next permutation (Algorithm from C++ STL)
int next_permutation(int *array, int size)
{
    int i = size - 2;
    while (i >= 0 && array[i] >= array[i + 1])
    {
        --i;
    }
    if (i < 0)
        return 0; // No more permutations

    int j = size - 1;
    while (array[j] <= array[i])
    {
        --j;
    }
    swap(&array[i], &array[j]);

    for (int k = i + 1, l = size - 1; k < l; ++k, --l)
    {
        swap(&array[k], &array[l]);
    }
    return 1;
}

// Function to parse the input file and build the distance matrix
void parse_input(FILE *file, int distances[MAX_LOCATIONS][MAX_LOCATIONS], int *num_locations)
{
    char buffer[256];
    char locations[MAX_LOCATIONS][30];
    *num_locations = 0;

    // Initialize distance matrix
    for (int i = 0; i < MAX_LOCATIONS; ++i)
    {
        for (int j = 0; j < MAX_LOCATIONS; ++j)
        {
            distances[i][j] = INF;
        }
    }

    while (fgets(buffer, sizeof(buffer), file) != NULL)
    {
        char loc1[30], loc2[30];
        int distance;
        if (sscanf(buffer, "%s to %s = %d", loc1, loc2, &distance) == 3)
        {
            int idx1 = -1, idx2 = -1;

            // Find indices for loc1 and loc2
            for (int i = 0; i < *num_locations; ++i)
            {
                if (strcmp(loc1, locations[i]) == 0)
                {
                    idx1 = i;
                }
                if (strcmp(loc2, locations[i]) == 0)
                {
                    idx2 = i;
                }
            }

            if (idx1 == -1)
            {
                idx1 = (*num_locations)++;
                strcpy(locations[idx1], loc1);
            }
            if (idx2 == -1)
            {
                idx2 = (*num_locations)++;
                strcpy(locations[idx2], loc2);
            }

            // Set the distances in both directions
            distances[idx1][idx2] = distance;
            distances[idx2][idx1] = distance;
        }
    }
}

// Function to calculate the distance of a route
int calculate_route_distance(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int route[], int num_locations)
{
    int distance = 0;
    for (int i = 0; i < num_locations - 1; ++i)
    {
        distance += distances[route[i]][route[i + 1]];
    }
    return distance;
}

// Function to find the shortest route using permutations
int find_shortest_route(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int num_locations)
{
    int route[MAX_LOCATIONS];
    int min_distance = INF;

    // Initialize route with the first permutation
    for (int i = 0; i < num_locations; ++i)
    {
        route[i] = i;
    }

    // Generate all permutations and calculate distances
    do
    {
        int distance = calculate_route_distance(distances, route, num_locations);
        if (distance < min_distance)
        {
            min_distance = distance;
        }
    } while (next_permutation(route, num_locations));

    return min_distance;
}

// Function to find the longest route using permutations
int find_longest_route(int distances[MAX_LOCATIONS][MAX_LOCATIONS], int num_locations)
{
    int route[MAX_LOCATIONS];
    int max_distance = 0;

    // Initialize route with the first permutation
    for (int i = 0; i < num_locations; ++i)
    {
        route[i] = i;
    }

    // Generate all permutations and calculate distances
    do
    {
        int distance = calculate_route_distance(distances, route, num_locations);
        if (distance > max_distance)
        {
            max_distance = distance;
        }
    } while (next_permutation(route, num_locations));

    return max_distance;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: missing input file\n");
        return EXIT_FAILURE;
    }

    char *filename = argv[1];
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        fprintf(stderr, "Error: cannot open file\n");
        return EXIT_FAILURE;
    }

    int distances[MAX_LOCATIONS][MAX_LOCATIONS];
    int num_locations;

    parse_input(file, distances, &num_locations);
    fclose(file);

    int shortest_distance = find_shortest_route(distances, num_locations);
    int longest_distance = find_longest_route(distances, num_locations);
    printf("Part 1: %d\n", shortest_distance);
    printf("Part 2: %d\n", longest_distance);

    return EXIT_SUCCESS;
}
