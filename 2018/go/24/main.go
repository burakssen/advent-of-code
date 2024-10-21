package main

import (
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type Group struct {
	n, hp, atk, init, side int
	immunities, weaknesses []string
	kind                   string
	target                 *Group
	targeted               bool
}

func (g *Group) effectivePower() int { return g.n * g.atk }

func (g *Group) damageTo(other *Group) int {
	if contains(other.immunities, g.kind) {
		return 0
	}
	damage := g.effectivePower()
	if contains(other.weaknesses, g.kind) {
		damage *= 2
	}
	return damage
}

func (g *Group) attack() {
	if g.target != nil && g.n > 0 {
		killed := min(g.damageTo(g.target)/g.target.hp, g.target.n)
		g.target.n -= killed
	}
}

func parseGroups(data string, boost int) []*Group {
	re := regexp.MustCompile(`(\d+) units each with (\d+) hit points (?:\(([^)]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)`)
	groups := []*Group{}

	for i, part := range strings.Split(data, "\n\n") {
		for _, line := range strings.Split(part, "\n") {
			if matches := re.FindStringSubmatch(line); matches != nil {
				g := &Group{
					n:          mustAtoi(matches[1]),
					hp:         mustAtoi(matches[2]),
					atk:        mustAtoi(matches[4]),
					kind:       matches[5],
					init:       mustAtoi(matches[6]),
					side:       i + 1,
					immunities: []string{},
					weaknesses: []string{},
				}
				if matches[3] != "" {
					for _, trait := range strings.Split(matches[3], "; ") {
						if strings.HasPrefix(trait, "immune to ") {
							g.immunities = strings.Split(strings.TrimPrefix(trait, "immune to "), ", ")
						} else if strings.HasPrefix(trait, "weak to ") {
							g.weaknesses = strings.Split(strings.TrimPrefix(trait, "weak to "), ", ")
						}
					}
				}
				if i == 0 {
					g.atk += boost
				}
				groups = append(groups, g)
			}
		}
	}
	return groups
}

func targetSelection(groups []*Group) {
	sort.Slice(groups, func(i, j int) bool {
		if ep := groups[i].effectivePower(); ep == groups[j].effectivePower() {
			return groups[i].init > groups[j].init
		}
		return groups[i].effectivePower() > groups[j].effectivePower()
	})

	for _, g := range groups {
		g.target = selectTarget(g, groups)
	}
}

func selectTarget(attacker *Group, groups []*Group) *Group {
	var target *Group
	for _, defender := range groups {
		if defender.targeted || attacker.side == defender.side || attacker.damageTo(defender) == 0 {
			continue
		}
		if target == nil || isBetterTarget(attacker, defender, target) {
			target = defender
		}
	}
	if target != nil {
		target.targeted = true
	}
	return target
}

func isBetterTarget(attacker, newTarget, currentTarget *Group) bool {
	newDamage, currentDamage := attacker.damageTo(newTarget), attacker.damageTo(currentTarget)
	if newDamage != currentDamage {
		return newDamage > currentDamage
	}
	if newEP, currentEP := newTarget.effectivePower(), currentTarget.effectivePower(); newEP != currentEP {
		return newEP > currentEP
	}
	return newTarget.init > currentTarget.init
}

func attackPhase(groups []*Group) []*Group {
	sort.Slice(groups, func(i, j int) bool { return groups[i].init > groups[j].init })
	for _, g := range groups {
		g.attack()
	}
	return filter(groups, func(g *Group) bool { return g.n > 0 })
}

func battle(data string, boost int) (winner string, units int, stalemate bool) {
	groups := parseGroups(data, boost)
	for rounds := 0; rounds < 2000; rounds++ {
		for _, g := range groups {
			g.target, g.targeted = nil, false
		}

		targetSelection(groups)

		totalUnitsBefore := sumUnits(groups)
		groups = attackPhase(groups)

		if sumUnits(groups) == totalUnitsBefore {
			stalemate = true
			break
		}

		if !hasMultipleSides(groups) {
			break
		}
	}

	if !hasMultipleSides(groups) {
		winner = "Immune System"
		if groups[0].side == 2 {
			winner = "Infection"
		}
		units = sumUnits(groups)
	} else {
		winner = "Tie"
	}
	return
}

func main() {

	if len(os.Args) != 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	filename := os.Args[1]

	data, err := os.ReadFile(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	_, outcome1, _ := battle(string(data), 0)
	fmt.Println("Part 1:", outcome1)

	low, high := 1, 20000

	for low < high {
		mid := (low + high) / 2
		winner, _, stalemate := battle(string(data), mid)
		if winner == "Immune System" && !stalemate {
			high = mid
		} else {
			low = mid + 1
		}
	}

	winnerFinal, unitsFinal, stalemateFinal := battle(string(data), low)
	if winnerFinal == "Immune System" && !stalemateFinal {
		fmt.Println("Part 2:", unitsFinal)
	} else {
		fmt.Println("Part 2: No solution found within the boost range.")
	}
}

// Utility functions
func mustAtoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

func filter(groups []*Group, f func(*Group) bool) []*Group {
	filtered := make([]*Group, 0)
	for _, g := range groups {
		if f(g) {
			filtered = append(filtered, g)
		}
	}
	return filtered
}

func hasMultipleSides(groups []*Group) bool {
	if len(groups) == 0 {
		return false
	}
	side := groups[0].side
	for _, g := range groups[1:] {
		if g.side != side {
			return true
		}
	}
	return false
}

func sumUnits(groups []*Group) int {
	total := 0
	for _, g := range groups {
		total += g.n
	}
	return total
}
