#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_RULES 100
#define MAX_MOLECULE_LEN 1000
#define MAX_REPLACEMENTS 10000

typedef struct
{
    char from[10];
    char to[10];
} Replacement;

Replacement replacements[MAX_RULES];
int replacement_count = 0;
char molecule[MAX_MOLECULE_LEN];
char *generated_molecules[MAX_REPLACEMENTS];
int generated_count = 0;

void read_input(FILE *rule_file, FILE *medicine_file)
{
    // Read the replacement rules
    while (fscanf(rule_file, "%s => %s", replacements[replacement_count].from, replacements[replacement_count].to) == 2)
    {
        replacement_count++;
    }

    // Read the starting molecule
    fscanf(medicine_file, "%s", molecule);
}

void add_unique_molecule(char *new_molecule)
{
    // Check if the new molecule is already in the generated molecules list
    for (int i = 0; i < generated_count; i++)
    {
        if (strcmp(generated_molecules[i], new_molecule) == 0)
        {
            return;
        }
    }
    // If not, add it
    generated_molecules[generated_count] = strdup(new_molecule);
    generated_count++;
}

void generate_new_molecules(void)
{
    char new_molecule[MAX_MOLECULE_LEN];
    int molecule_len = strlen(molecule);

    for (int i = 0; i < replacement_count; i++)
    {
        int from_len = strlen(replacements[i].from);
        int to_len = strlen(replacements[i].to);

        for (int j = 0; j <= molecule_len - from_len; j++)
        {
            if (strncmp(&molecule[j], replacements[i].from, from_len) == 0)
            {
                // Create new molecule
                snprintf(new_molecule, j + 1, "%s", molecule);                                                       // copy part before replacement
                snprintf(new_molecule + j, to_len + 1, "%s", replacements[i].to);                                    // copy replacement part
                snprintf(new_molecule + j + to_len, molecule_len - j - from_len + 1, "%s", molecule + j + from_len); // copy part after replacement

                // Add new molecule to the list of generated molecules
                add_unique_molecule(new_molecule);
            }
        }
    }
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        printf("Usage: %s <rule_filename> <medicine_filename>\n", argv[0]);
        return 1;
    }

    char *rule_filename = argv[1];
    char *medicine_filename = argv[2];

    FILE *rule_file = fopen(rule_filename, "r");
    if (rule_file == NULL)
    {
        perror(rule_filename);
        return 1;
    }

    FILE *medicine_file = fopen(medicine_filename, "r");
    if (medicine_file == NULL)
    {
        perror(medicine_filename);
        fclose(rule_file);
        return 1;
    }

    read_input(rule_file, medicine_file);

    fclose(rule_file);
    fclose(medicine_file);

    generate_new_molecules();

    printf("Part 1: %d\n", generated_count);

    // Free allocated memory for generated molecules
    for (int i = 0; i < generated_count; i++)
    {
        free(generated_molecules[i]);
    }

    return 0;
}
