#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file)
    {
        printf("Error: Could not open file %s\n", argv[1]);
        return 1;
    }

    int paper_size = 0, ribbon_length = 0, a, b, c;
    char line[256];

    while (fgets(line, sizeof(line), file))
    {
        sscanf(line, "%dx%dx%d", &a, &b, &c);

        // Calculate the area and the smallest side in one pass
        int ab = a * b, bc = b * c, ca = c * a;
        int smallest = ab < bc ? (ab < ca ? ab : ca) : (bc < ca ? bc : ca);
        paper_size += 2 * (ab + bc + ca) + smallest;

        // Calculate ribbon length
        int min_perimeter = 2 * (a + b + c - (a > b ? (a > c ? a : c) : (b > c ? b : c)));
        ribbon_length += min_perimeter + a * b * c;
    }

    printf("Part 1: %d\n", paper_size);
    printf("Part 2: %d\n", ribbon_length);

    fclose(file);
    return 0;
}
