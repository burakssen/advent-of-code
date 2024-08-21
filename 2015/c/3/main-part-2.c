#include <stdio.h>
#include <stdlib.h>

#define OFFSET 500
#define GRID_SIZE 1000

void update_position(int *x, int *y, char direction)
{
    switch (direction)
    {
    case '^':
        (*y)++;
        break;
    case 'v':
        (*y)--;
        break;
    case '>':
        (*x)++;
        break;
    case '<':
        (*x)--;
        break;
    }
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int x_santa = 0, y_santa = 0;
    int x_robo_santa = 0, y_robo_santa = 0;
    int grid[GRID_SIZE][GRID_SIZE] = {0};
    grid[x_santa + OFFSET][y_santa + OFFSET] = 1;
    grid[x_robo_santa + OFFSET][y_robo_santa + OFFSET] = 1;

    int is_santa_turn = 1;
    int c;

    while ((c = fgetc(file)) != EOF)
    {
        if (is_santa_turn)
        {
            update_position(&x_santa, &y_santa, c);
            grid[x_santa + OFFSET][y_santa + OFFSET] = 1;
        }
        else
        {
            update_position(&x_robo_santa, &y_robo_santa, c);
            grid[x_robo_santa + OFFSET][y_robo_santa + OFFSET] = 1;
        }

        is_santa_turn = !is_santa_turn;
    }

    fclose(file);

    int count = 0;
    for (int i = 0; i < GRID_SIZE; i++)
    {
        for (int j = 0; j < GRID_SIZE; j++)
        {
            if (grid[i][j])
            {
                count++;
            }
        }
    }

    printf("Part 2: %d\n", count);
    return EXIT_SUCCESS;
}
