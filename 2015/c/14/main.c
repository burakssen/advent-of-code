#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Reindeer
{
    char name[20];
    int speed;
    int fly_time;
    int rest_time;
} Reindeer;

int calculate_distance(int speed, int fly_time, int rest_time, int time)
{
    int cycle_time = fly_time + rest_time;
    int full_cycles = time / cycle_time;
    int remaining_time = time % cycle_time;

    int distance = full_cycles * speed * fly_time;
    if (remaining_time >= fly_time)
    {
        distance += speed * fly_time;
    }
    else
    {
        distance += speed * remaining_time;
    }

    return distance;
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

    Reindeer reindeers[10];
    int points[10] = {0};
    int distances[10] = {0};
    int reindeer_count = 0;
    char line[256];

    while (fgets(line, sizeof(line), file))
    {
        sscanf(line, "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.",
               reindeers[reindeer_count].name,
               &reindeers[reindeer_count].speed,
               &reindeers[reindeer_count].fly_time,
               &reindeers[reindeer_count].rest_time);
        reindeer_count++;
    }
    fclose(file);

    int total_time = 2503;
    int max_distance = 0;

    for (int t = 1; t <= total_time; t++)
    {

        for (int i = 0; i < reindeer_count; i++)
        {
            distances[i] = calculate_distance(reindeers[i].speed, reindeers[i].fly_time, reindeers[i].rest_time, t);
            if (distances[i] > max_distance)
            {
                max_distance = distances[i];
            }
        }

        for (int i = 0; i < reindeer_count; i++)
        {
            if (distances[i] == max_distance)
            {
                points[i]++;
            }
        }
    }

    int max_points = 0;
    for (int i = 0; i < reindeer_count; i++)
    {
        if (points[i] > max_points)
        {
            max_points = points[i];
        }
    }

    printf("Part 1: %d\n", max_distance);
    printf("Part 2: %d\n", max_points);

    return EXIT_SUCCESS;
}
