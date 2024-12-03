package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
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
	re := regexp.MustCompile(`mul\((\d+),(\d+)\)`)
	var sum int
	for scanner.Scan() {
		line := scanner.Text()
		muls := re.FindAllStringSubmatch(line, -1)
		for _, mul := range muls {
			n, err := strconv.Atoi(mul[1])
			if err != nil {
				panic("cannot convert to int")
			}
			m, err := strconv.Atoi(mul[2])
			if err != nil {
				panic("cannot convert to int")
			}
			sum += n * m
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}
