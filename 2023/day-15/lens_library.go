package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	fmt.Println(part1())
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	steps := strings.Split(scanner.Text(), ",")

	var sum int
	for _, s := range steps {
		sum += hash(s)
	}

	return sum
}

func hash(s string) int {
	var h int
	for i := range s {
		h = ((h + int(s[i])) * 17) & 0b11111111
	}

	return h
}
