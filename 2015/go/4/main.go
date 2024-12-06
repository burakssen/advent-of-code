package main

import (
	"crypto/md5"
	"fmt"
	"os"
	"strconv"
)

func main() {
	// get command line arguments
	args := os.Args[1:]

	// check if there are any arguments
	if len(args) == 0 {
		// if no arguments, print usage
		println("Usage: go run main.go <input.txt>")
		return
	}

	filename := args[0]

	// read the file
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}

	// close the file when the function returns
	defer file.Close()

	var line string
	_, err = fmt.Fscanln(file, &line)
	if err != nil {
		panic(err)
	}

	var i = 0
	var leading_5_found = false

	for {
		var input string
		input = line + "" + strconv.FormatInt(int64(i), 10)

		// calculate the hash
		hash := md5.Sum([]byte(input))

		// check if the hash starts with 5 zeros
		if hash[0] == 0 && hash[1] == 0 && hash[2] < 16 && !leading_5_found {
			fmt.Printf("Part 1: %d\n", i)
			leading_5_found = true
		}

		// check if the hash starts with 6 zeros
		if hash[0] == 0 && hash[1] == 0 && hash[2] == 0 {
			fmt.Printf("Part 2: %d\n", i)
			break
		}

		// increment the number
		i++
	}
}
