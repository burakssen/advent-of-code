#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct
{
    int cost;
    int damage;
    int armor;
} Item;

typedef struct
{
    int hit_points;
    int damage;
    int armor;
} Character;

Item weapons[] = {
    {8, 4, 0},
    {10, 5, 0},
    {25, 6, 0},
    {40, 7, 0},
    {74, 8, 0}};

Item armors[] = {
    {0, 0, 0}, // No armor
    {13, 0, 1},
    {31, 0, 2},
    {53, 0, 3},
    {75, 0, 4},
    {102, 0, 5}};

Item rings[] = {
    {0, 0, 0}, // No ring
    {25, 1, 0},
    {50, 2, 0},
    {100, 3, 0},
    {20, 0, 1},
    {40, 0, 2},
    {80, 0, 3}};

bool player_wins(Character player, Character boss)
{
    int player_damage = player.damage - boss.armor;
    int boss_damage = boss.damage - player.armor;
    if (player_damage < 1)
        player_damage = 1;
    if (boss_damage < 1)
        boss_damage = 1;

    int player_turns = (boss.hit_points + player_damage - 1) / player_damage;
    int boss_turns = (player.hit_points + boss_damage - 1) / boss_damage;

    return player_turns <= boss_turns;
}

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL)
    {
        perror("Error opening file");
        return 1;
    }

    Character boss;
    fscanf(file, "Hit Points: %d\nDamage: %d\nArmor: %d", &boss.hit_points, &boss.damage, &boss.armor);
    fclose(file);

    Character player = {100, 0, 0};
    int min_gold = 10000; // Large initial value for Part 1
    int max_gold = 0;     // Initial value for Part 2

    // Try all combinations of items
    for (size_t w = 0; w < sizeof(weapons) / sizeof(Item); w++)
    {
        for (size_t a = 0; a < sizeof(armors) / sizeof(Item); a++)
        {
            for (size_t r1 = 0; r1 < sizeof(rings) / sizeof(Item); r1++)
            {
                for (size_t r2 = r1 + 1; r2 < sizeof(rings) / sizeof(Item); r2++)
                {
                    player.damage = weapons[w].damage + armors[a].damage + rings[r1].damage + rings[r2].damage;
                    player.armor = weapons[w].armor + armors[a].armor + rings[r1].armor + rings[r2].armor;
                    int gold_spent = weapons[w].cost + armors[a].cost + rings[r1].cost + rings[r2].cost;

                    // Check for Part 1
                    if (player_wins(player, boss) && gold_spent < min_gold)
                    {
                        min_gold = gold_spent;
                    }

                    // Check for Part 2
                    if (!player_wins(player, boss) && gold_spent > max_gold)
                    {
                        max_gold = gold_spent;
                    }
                }
            }
        }
    }

    printf("Part 1: %d\n", min_gold);
    printf("Part 2: %d\n", max_gold);
    return 0;
}
