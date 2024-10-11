package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
)

type Record struct {
	timestamp time.Time
	action    string
}

type GuardSleep struct {
	totalSleep int
	minutes    [60]int
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	filename := os.Args[1]
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var records []Record
	for scanner.Scan() {
		line := scanner.Text()
		timestamp, _ := time.Parse("2006-01-02 15:04", line[1:17])
		records = append(records, Record{timestamp, line[19:]})
	}

	sort.Slice(records, func(i, j int) bool {
		return records[i].timestamp.Before(records[j].timestamp)
	})

	guardSleep := analyzeRecords(records)

	sleepiestGuard, sleepiestMinute := findSleepiestGuard(guardSleep)
	fmt.Printf("Part 1: %d\n", sleepiestGuard*sleepiestMinute)

	consistentGuard, consistentMinute := findMostConsistentGuard(guardSleep)
	fmt.Printf("Part 2: %d\n", consistentGuard*consistentMinute)
}

func analyzeRecords(records []Record) map[int]*GuardSleep {
	guardSleep := make(map[int]*GuardSleep)
	var currentGuard int
	var sleepStart time.Time

	for _, record := range records {
		switch {
		case strings.Contains(record.action, "Guard"):
			fmt.Sscanf(record.action, "Guard #%d", &currentGuard)
		case record.action == "falls asleep":
			sleepStart = record.timestamp
		case record.action == "wakes up":
			sleep := guardSleep[currentGuard]
			if sleep == nil {
				sleep = &GuardSleep{}
				guardSleep[currentGuard] = sleep
			}
			for i := sleepStart.Minute(); i < record.timestamp.Minute(); i++ {
				sleep.minutes[i]++
				sleep.totalSleep++
			}
		}
	}
	return guardSleep
}

func findSleepiestGuard(guardSleep map[int]*GuardSleep) (int, int) {
	var sleepiestGuard, sleepiestMinute, maxSleep int
	for guard, sleep := range guardSleep {
		if sleep.totalSleep > maxSleep {
			maxSleep, sleepiestGuard = sleep.totalSleep, guard
		}
	}
	sleepiestMinute = maxMinute(guardSleep[sleepiestGuard].minutes)
	return sleepiestGuard, sleepiestMinute
}

func findMostConsistentGuard(guardSleep map[int]*GuardSleep) (int, int) {
	var consistentGuard, consistentMinute, maxFrequency int
	for guard, sleep := range guardSleep {
		if minute := maxMinute(sleep.minutes); sleep.minutes[minute] > maxFrequency {
			maxFrequency, consistentGuard, consistentMinute = sleep.minutes[minute], guard, minute
		}
	}
	return consistentGuard, consistentMinute
}

func maxMinute(minutes [60]int) int {
	var maxMinute, maxCount int
	for minute, count := range minutes {
		if count > maxCount {
			maxMinute, maxCount = minute, count
		}
	}
	return maxMinute
}
