#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    char *filename = argv[1];
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        printf("Error: Could not open file %s\n", filename);
        return 1;
    }

    int total = 0;
    // loop each line in the file (each line has 3 values and they are separated by a x)
    char line[256];
    while (fgets(line, sizeof(line), file))
    {
        int a, b, c;
        sscanf(line, "%dx%dx%d", &a, &b, &c);

        // calculate the area of the box
        int area = 2 * a * b + 2 * b * c + 2 * c * a;

        // find the smallest side
        int smallest = a * b;
        if (b * c < smallest)
        {
            smallest = b * c;
        }
        if (c * a < smallest)
        {
            smallest = c * a;
        }

        // add the area of the smallest side
        area += smallest;
        total += area;
    }

    printf("%d\n", total);

    fclose(file);
    return 0;
}
