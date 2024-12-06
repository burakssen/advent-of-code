#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1000

unsigned long long generate_next_code(unsigned long long previous_code)
{
    return (previous_code * 252533ULL) % 33554393ULL;
}

unsigned long long find_code_at_position(int row, int col)
{
    int position = 1;
    for (int i = 1; i < row + col - 1; i++)
    {
        position += i;
    }
    position += col - 1;

    unsigned long long code = 20151125ULL;
    for (int i = 1; i < position; i++)
    {
        code = generate_next_code(code);
    }

    return code;
}

int parse_input(const char *line, int *row, int *col)
{
    return sscanf(line, "To continue, please consult the code grid in the manual. Enter the code at row %d, column %d.", row, col);
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    if (fgets(line, sizeof(line), file) == NULL)
    {
        fprintf(stderr, "Error reading file or file is empty\n");
        fclose(file);
        return 1;
    }

    int row, col;
    if (parse_input(line, &row, &col) != 2)
    {
        fprintf(stderr, "Invalid input format\n");
        fclose(file);
        return 1;
    }

    unsigned long long result = find_code_at_position(row, col);
    printf("Part 1: %llu\n", result);
    printf("Part 2: Merry Christmas! :D\n");

    fclose(file);
    return 0;
}
