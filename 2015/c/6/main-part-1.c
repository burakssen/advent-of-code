#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define GRID_SIZE 1000
#define ACTION_SIZE 10

// Helper function to parse the action from the input line
void parse_action(const char *line, char *action, int *startx, int *starty, int *endx, int *endy)
{
    if (line[0] == 't' && line[1] == 'u') // Handles "turn on/off" case
        sscanf(line, "turn %s %d,%d through %d,%d", action, startx, starty, endx, endy);
    else // Handles "toggle" case
        sscanf(line, "%s %d,%d through %d,%d", action, startx, starty, endx, endy);
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

    bool lights[GRID_SIZE][GRID_SIZE] = {false};
    char line[256]; // Assuming lines are less than 256 characters

    while (fgets(line, sizeof(line), file))
    {
        char action[ACTION_SIZE];
        int startx, starty, endx, endy;

        parse_action(line, action, &startx, &starty, &endx, &endy);

        // Clamp the coordinates within bounds (optional, but good for robustness)
        startx = startx < 0 ? 0 : (startx >= GRID_SIZE ? GRID_SIZE - 1 : startx);
        starty = starty < 0 ? 0 : (starty >= GRID_SIZE ? GRID_SIZE - 1 : starty);
        endx = endx < 0 ? 0 : (endx >= GRID_SIZE ? GRID_SIZE - 1 : endx);
        endy = endy < 0 ? 0 : (endy >= GRID_SIZE ? GRID_SIZE - 1 : endy);

        for (int i = startx; i <= endx; i++)
        {
            for (int j = starty; j <= endy; j++)
            {
                if (strcmp(action, "on") == 0)
                    lights[i][j] = true;
                else if (strcmp(action, "off") == 0)
                    lights[i][j] = false;
                else // action is "toggle"
                    lights[i][j] = !lights[i][j];
            }
        }
    }

    fclose(file);

    // Count the number of lights that are on
    int count = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            if (lights[i][j])
                count++;
        }
    }

    printf("Part 1: %d\n", count);
    return EXIT_SUCCESS;
}
