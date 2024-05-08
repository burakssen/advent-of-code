#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

void get_action(char *line, char *action, int *startx, int *starty, int *endx, int *endy)
{
    char turn[10];
    if (line[0] == 't' && line[1] == 'u')
        sscanf(line, "%s %s %d,%d through %d,%d", turn, action, startx, starty, endx, endy);
    else
        sscanf(line, "%s %d,%d through %d,%d", action, startx, starty, endx, endy);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <input.txt>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        printf("Error: Unable to open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    bool lights[1000][1000] = {false};

    while ((read = getline(&line, &len, file)) != -1)
    {
        char action[10];
        int startx, starty, endx, endy;

        get_action(line, action, &startx, &starty, &endx, &endy);

        for (int i = startx; i <= endx; i++)
        {
            for (int j = starty; j <= endy; j++)
            {
                if (strcmp(action, "on") == 0)
                    lights[i][j] = true;
                else if (strcmp(action, "off") == 0)
                    lights[i][j] = false;
                else
                    lights[i][j] = !lights[i][j];
            }
        }
    }

    int count = 0;
    for (int i = 0; i < 1000; i++)
    {
        for (int j = 0; j < 1000; j++)
        {
            if (lights[i][j])
                count++;
        }
    }

    printf("Lights on: %d\n", count);

    free(line);

    return EXIT_SUCCESS;
}
