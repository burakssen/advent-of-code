#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_INSTRUCTION_LENGTH 10
#define GRID_SIZE 1000 // Adjust this based on expected grid size

typedef struct
{
    int x;
    int y;
} Position;

typedef enum
{
    NORTH,
    EAST,
    SOUTH,
    WEST
} Direction;

void turn(Direction *dir, char turn)
{
    if (turn == 'L')
    {
        *dir = (*dir - 1 + 4) % 4;
    }
    else if (turn == 'R')
    {
        *dir = (*dir + 1) % 4;
    }
}

void move(Position *pos, Direction dir, int steps)
{
    switch (dir)
    {
    case NORTH:
        pos->y += steps;
        break;
    case EAST:
        pos->x += steps;
        break;
    case SOUTH:
        pos->y -= steps;
        break;
    case WEST:
        pos->x -= steps;
        break;
    }
}

bool move_and_check(Position *pos, Direction dir, int steps, bool grid[GRID_SIZE][GRID_SIZE], Position *first_revisit)
{
    int dx = 0, dy = 0;
    switch (dir)
    {
    case NORTH:
        dy = 1;
        break;
    case EAST:
        dx = 1;
        break;
    case SOUTH:
        dy = -1;
        break;
    case WEST:
        dx = -1;
        break;
    }

    for (int i = 0; i < steps; i++)
    {
        pos->x += dx;
        pos->y += dy;

        int grid_x = pos->x + GRID_SIZE / 2;
        int grid_y = pos->y + GRID_SIZE / 2;

        if (grid_x < 0 || grid_x >= GRID_SIZE || grid_y < 0 || grid_y >= GRID_SIZE)
        {
            fprintf(stderr, "Error: Position out of grid bounds\n");
            exit(EXIT_FAILURE);
        }

        if (grid[grid_x][grid_y])
        {
            if (first_revisit->x == 0 && first_revisit->y == 0)
            {
                *first_revisit = *pos;
                return true;
            }
        }
        else
        {
            grid[grid_x][grid_y] = true;
        }
    }
    return false;
}

int manhattan_distance(Position pos)
{
    return abs(pos.x) + abs(pos.y);
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    char instructions[MAX_INSTRUCTIONS][MAX_INSTRUCTION_LENGTH];
    int instruction_count = 0;

    while (fscanf(file, "%9[^,], ", instructions[instruction_count]) == 1)
    {
        instruction_count++;
        if (instruction_count >= MAX_INSTRUCTIONS)
        {
            fprintf(stderr, "Error: Too many instructions\n");
            fclose(file);
            return EXIT_FAILURE;
        }
    }

    fclose(file);

    // Part One
    Position pos1 = {0, 0};
    Direction dir1 = NORTH;

    for (int i = 0; i < instruction_count; i++)
    {
        char turn_dir = instructions[i][0];
        int steps = atoi(instructions[i] + 1);

        turn(&dir1, turn_dir);
        move(&pos1, dir1, steps);
    }

    int final_distance = manhattan_distance(pos1);
    printf("Part 1: %d\n", final_distance);

    // Part Two
    Position pos2 = {0, 0};
    Direction dir2 = NORTH;
    Position first_revisit = {0, 0};
    bool grid[GRID_SIZE][GRID_SIZE] = {{false}};
    grid[GRID_SIZE / 2][GRID_SIZE / 2] = true; // Mark starting position as visited

    for (int i = 0; i < instruction_count; i++)
    {
        char turn_dir = instructions[i][0];
        int steps = atoi(instructions[i] + 1);

        turn(&dir2, turn_dir);
        if (move_and_check(&pos2, dir2, steps, grid, &first_revisit))
        {
            break;
        }
    }

    if (first_revisit.x != 0 || first_revisit.y != 0)
    {
        int revisit_distance = manhattan_distance(first_revisit);
        printf("Part 2: %d\n", revisit_distance);
    }
    else
    {
        printf("Part 2: No location was visited twice.\n");
    }

    return EXIT_SUCCESS;
}
