#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef struct
{
    int hit_points;
    int mana;
    int armor;
    int boss_hp;
    int boss_damage;
    int shield_timer;
    int poison_timer;
    int recharge_timer;
    int mana_spent;
} GameState;

typedef enum
{
    MAGIC_MISSILE,
    DRAIN,
    SHIELD,
    POISON,
    RECHARGE
} Spell;

const int SPELL_COSTS[] = {53, 73, 113, 173, 229};

void apply_effects(GameState *state)
{
    if (state->shield_timer > 0)
    {
        state->shield_timer--;
        state->armor = 7;
    }
    else
    {
        state->armor = 0;
    }
    if (state->poison_timer > 0)
    {
        state->poison_timer--;
        state->boss_hp -= 3;
    }
    if (state->recharge_timer > 0)
    {
        state->recharge_timer--;
        state->mana += 101;
    }
}

bool cast_spell(GameState *state, Spell spell)
{
    if (state->mana < SPELL_COSTS[spell])
    {
        return false;
    }
    state->mana -= SPELL_COSTS[spell];
    state->mana_spent += SPELL_COSTS[spell];
    switch (spell)
    {
    case MAGIC_MISSILE:
        state->boss_hp -= 4;
        break;
    case DRAIN:
        state->boss_hp -= 2;
        state->hit_points += 2;
        break;
    case SHIELD:
        if (state->shield_timer > 0)
            return false;
        state->shield_timer = 6;
        break;
    case POISON:
        if (state->poison_timer > 0)
            return false;
        state->poison_timer = 6;
        break;
    case RECHARGE:
        if (state->recharge_timer > 0)
            return false;
        state->recharge_timer = 5;
        break;
    }
    return true;
}

int simulate_battle(GameState state, bool player_turn, int best_so_far, bool hard_mode)
{
    if (state.mana_spent >= best_so_far)
    {
        return INT_MAX;
    }

    if (player_turn && hard_mode)
    {
        state.hit_points--;
        if (state.hit_points <= 0)
        {
            return INT_MAX;
        }
    }

    apply_effects(&state);

    if (state.boss_hp <= 0)
    {
        return state.mana_spent;
    }

    if (player_turn)
    {
        if (state.hit_points <= 0)
        {
            return INT_MAX;
        }
        int min_mana = INT_MAX;
        for (Spell spell = MAGIC_MISSILE; spell <= RECHARGE; spell++)
        {
            GameState new_state = state;
            if (cast_spell(&new_state, spell))
            {
                int result = simulate_battle(new_state, false, MIN(best_so_far, min_mana), hard_mode);
                min_mana = MIN(min_mana, result);
            }
        }
        return min_mana;
    }
    else
    {
        state.hit_points -= MAX(1, state.boss_damage - state.armor);
        return simulate_battle(state, true, best_so_far, hard_mode);
    }
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

    GameState initial_state = {
        .hit_points = 50,
        .mana = 500,
        .armor = 0,
        .boss_hp = 0,
        .boss_damage = 0,
        .shield_timer = 0,
        .poison_timer = 0,
        .recharge_timer = 0,
        .mana_spent = 0};

    fscanf(file, "Hit Points: %d\nDamage: %d", &initial_state.boss_hp, &initial_state.boss_damage);
    fclose(file);

    int result_part1 = simulate_battle(initial_state, true, INT_MAX, false);
    printf("Part 1: %d\n", result_part1);

    int result_part2 = simulate_battle(initial_state, true, INT_MAX, true);
    printf("Part 2: %d\n", result_part2);

    return 0;
}
