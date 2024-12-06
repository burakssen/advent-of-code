package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// countChars calculates the number of characters in the code and the
// number of characters in the string representation, excluding escape sequences.
func countChars(s string, codeChars, stringChars *int) {
	*codeChars += len(s)

	for i := 1; i < len(s)-1; i++ { // Skip first and last quote
		if s[i] == '\\' {
			if i+1 < len(s)-1 {
				switch s[i+1] {
				case '\\', '"':
					*stringChars++
					i++
				case 'x':
					if i+3 < len(s)-1 {
						*stringChars++
						i += 3
					}
				default:
					*stringChars++
				}
			}
		} else {
			*stringChars++
		}
	}
}

// encodeString returns an encoded version of the input string and its length.
func encodeString(s string) (string, int) {
	var b strings.Builder
	b.WriteString("\"")

	for _, ch := range s {
		if ch == '"' || ch == '\\' {
			b.WriteByte('\\')
		}
		b.WriteByte(byte(ch))
	}

	b.WriteString("\"")
	return b.String(), b.Len()
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "missing input file")
		os.Exit(1)
	}

	f, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "cannot open file: %v\n", err)
		os.Exit(1)
	}
	defer f.Close()

	var (
		totalCodeChars   int
		totalStringChars int
		totalOrigChars   int
		totalEncChars    int
	)

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		// Part 1: Count characters
		countChars(line, &totalCodeChars, &totalStringChars)

		// Part 2: Encode string
		totalOrigChars += len(line)

		_, encLen := encodeString(line)
		totalEncChars += encLen
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "error reading file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", totalCodeChars-totalStringChars)
	fmt.Printf("Part 2: %d\n", totalEncChars-totalOrigChars)
}
