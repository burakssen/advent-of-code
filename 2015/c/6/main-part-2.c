#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define GRID_SIZE 1000
#define LINE_SIZE 256
#define ACTION_SIZE 10

// Parse the action and coordinates from the input line
void parse_action(const char *line, char *action, int *startx, int *starty, int *endx, int *endy)
{
    if (strncmp(line, "toggle", 6) == 0)
        sscanf(line, "toggle %d,%d through %d,%d", startx, starty, endx, endy);
    else
        sscanf(line, "turn %s %d,%d through %d,%d", action, startx, starty, endx, endy);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input.txt>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int lights[GRID_SIZE][GRID_SIZE] = {0};
    char line[LINE_SIZE];

    while (fgets(line, sizeof(line), file))
    {
        char action[ACTION_SIZE] = {0};
        int startx, starty, endx, endy;

        parse_action(line, action, &startx, &starty, &endx, &endy);

        for (int i = startx; i <= endx; i++)
        {
            for (int j = starty; j <= endy; j++)
            {
                if (strncmp(action, "on", 2) == 0)
                    lights[i][j]++;
                else if (strncmp(action, "off", 3) == 0)
                    lights[i][j] = lights[i][j] > 0 ? lights[i][j] - 1 : 0;
                else
                    lights[i][j] += 2;
            }
        }
    }

    fclose(file);

    // Calculate total brightness
    int total_brightness = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            total_brightness += lights[i][j];
        }
    }

    printf("Part 2: %d\n", total_brightness);
    return EXIT_SUCCESS;
}
