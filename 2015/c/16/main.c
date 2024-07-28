#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_AUNTS 500

typedef struct
{
    int id;
    int children;
    int cats;
    int samoyeds;
    int pomeranians;
    int akitas;
    int vizslas;
    int goldfish;
    int trees;
    int cars;
    int perfumes;
} AuntSue;

void initializeAunt(AuntSue *aunt)
{
    aunt->children = -1;
    aunt->cats = -1;
    aunt->samoyeds = -1;
    aunt->pomeranians = -1;
    aunt->akitas = -1;
    aunt->vizslas = -1;
    aunt->goldfish = -1;
    aunt->trees = -1;
    aunt->cars = -1;
    aunt->perfumes = -1;
}

int parseLine(char *line, AuntSue *aunt)
{
    char *token;
    token = strtok(line, " ");
    token = strtok(NULL, " "); // Skip "Sue"
    aunt->id = atoi(token);

    initializeAunt(aunt);

    while ((token = strtok(NULL, " :,,")))
    {
        if (strcmp(token, "children") == 0)
        {
            aunt->children = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "cats") == 0)
        {
            aunt->cats = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "samoyeds") == 0)
        {
            aunt->samoyeds = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "pomeranians") == 0)
        {
            aunt->pomeranians = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "akitas") == 0)
        {
            aunt->akitas = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "vizslas") == 0)
        {
            aunt->vizslas = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "goldfish") == 0)
        {
            aunt->goldfish = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "trees") == 0)
        {
            aunt->trees = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "cars") == 0)
        {
            aunt->cars = atoi(strtok(NULL, " ,"));
        }
        else if (strcmp(token, "perfumes") == 0)
        {
            aunt->perfumes = atoi(strtok(NULL, " ,"));
        }
    }
    return 1;
}

int matchesMFCSAMPart1(AuntSue *aunt)
{
    if ((aunt->children != -1 && aunt->children != 3) ||
        (aunt->cats != -1 && aunt->cats != 7) ||
        (aunt->samoyeds != -1 && aunt->samoyeds != 2) ||
        (aunt->pomeranians != -1 && aunt->pomeranians != 3) ||
        (aunt->akitas != -1 && aunt->akitas != 0) ||
        (aunt->vizslas != -1 && aunt->vizslas != 0) ||
        (aunt->goldfish != -1 && aunt->goldfish != 5) ||
        (aunt->trees != -1 && aunt->trees != 3) ||
        (aunt->cars != -1 && aunt->cars != 2) ||
        (aunt->perfumes != -1 && aunt->perfumes != 1))
    {
        return 0;
    }
    return 1;
}

int matchesMFCSAMPart2(AuntSue *aunt)
{
    if ((aunt->children != -1 && aunt->children != 3) ||
        (aunt->cats != -1 && aunt->cats <= 7) || // Cats > 7
        (aunt->samoyeds != -1 && aunt->samoyeds != 2) ||
        (aunt->pomeranians != -1 && aunt->pomeranians >= 3) || // Pomeranians < 3
        (aunt->akitas != -1 && aunt->akitas != 0) ||
        (aunt->vizslas != -1 && aunt->vizslas != 0) ||
        (aunt->goldfish != -1 && aunt->goldfish >= 5) || // Goldfish < 5
        (aunt->trees != -1 && aunt->trees <= 3) ||       // Trees > 3
        (aunt->cars != -1 && aunt->cars != 2) ||
        (aunt->perfumes != -1 && aunt->perfumes != 1))
    {
        return 0;
    }
    return 1;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    AuntSue aunts[MAX_AUNTS];
    int auntCount = 0;

    while (fgets(line, sizeof(line), file))
    {
        if (auntCount >= MAX_AUNTS)
        {
            printf("Too many aunts\n");
            return 1;
        }
        parseLine(line, &aunts[auntCount]);
        auntCount++;
    }

    fclose(file);

    int part_1 = 0;
    int part_2 = 0;

    for (int i = 0; i < auntCount; i++)
    {
        if (matchesMFCSAMPart1(&aunts[i]))
            part_1 = aunts[i].id;
        if (matchesMFCSAMPart2(&aunts[i]))
            part_2 = aunts[i].id;
    }

    printf("Part 1: %d\n", part_1);
    printf("Part 2: %d\n", part_2);

    return 0;
}
