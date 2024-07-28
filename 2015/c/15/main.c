#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct
{
    char name[20];
    int capacity;
    int durability;
    int flavor;
    int texture;
    int calories;
} Ingredient;

void calculate_score(Ingredient ingredients[], int amounts[], int ingredient_count, int *max_score, bool part2)
{
    int total_capacity = 0, total_durability = 0, total_flavor = 0, total_texture = 0, total_calories = 0;

    for (int i = 0; i < ingredient_count; i++)
    {
        total_capacity += ingredients[i].capacity * amounts[i];
        total_durability += ingredients[i].durability * amounts[i];
        total_flavor += ingredients[i].flavor * amounts[i];
        total_texture += ingredients[i].texture * amounts[i];
        total_calories += ingredients[i].calories * amounts[i];
    }

    if (part2 && total_calories != 500)
        return;

    if (total_capacity < 0)
        total_capacity = 0;
    if (total_durability < 0)
        total_durability = 0;
    if (total_flavor < 0)
        total_flavor = 0;
    if (total_texture < 0)
        total_texture = 0;

    int score = total_capacity * total_durability * total_flavor * total_texture;

    if (score > *max_score)
    {
        *max_score = score;
    }
}

int find_best_combination(Ingredient ingredients[], int ingredient_count, bool part2)
{
    int max_score = 0;
    int amounts[ingredient_count];

    for (amounts[0] = 0; amounts[0] <= 100; amounts[0]++)
    {
        for (amounts[1] = 0; amounts[1] <= 100 - amounts[0]; amounts[1]++)
        {
            for (amounts[2] = 0; amounts[2] <= 100 - amounts[0] - amounts[1]; amounts[2]++)
            {
                amounts[3] = 100 - amounts[0] - amounts[1] - amounts[2];
                calculate_score(ingredients, amounts, ingredient_count, &max_score, part2);
            }
        }
    }

    return max_score;
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

    Ingredient ingredients[4];
    int ingredient_count = 0;
    char line[256];

    while (fgets(line, sizeof(line), file))
    {
        sscanf(line, "%s capacity %d, durability %d, flavor %d, texture %d, calories %d",
               ingredients[ingredient_count].name,
               &ingredients[ingredient_count].capacity,
               &ingredients[ingredient_count].durability,
               &ingredients[ingredient_count].flavor,
               &ingredients[ingredient_count].texture,
               &ingredients[ingredient_count].calories);
        ingredient_count++;
    }
    fclose(file);

    int max_score = find_best_combination(ingredients, ingredient_count, false);
    printf("Part 1: %d\n", max_score);
    max_score = find_best_combination(ingredients, ingredient_count, true);
    printf("Part 2: %d\n", max_score);

    return EXIT_SUCCESS;
}
