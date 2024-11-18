package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(sumOfPoints())
}

func sumOfPoints() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	sum := 0

	for scanner.Scan() {
		line := scanner.Text()
		if err != nil {
			panic("cannot read line")
		}
		parts := strings.Split(line, ":")
		if len(parts) < 2 {
			panic("invalid input")
		}

		numbers := strings.Split(parts[1], "|")
		if len(numbers) < 2 {
			panic("invalid number parts")
		}

		var beforePipe, afterPipe = numbers[0], numbers[1]

		winning := make(map[int]struct{})

		for _, n := range strings.Fields(beforePipe) {
			num, err := strconv.Atoi(n)
			if err != nil {
				panic("cannot parse number")
			}
			winning[num] = struct{}{}
		}

		score := 0
		for _, n := range strings.Fields(afterPipe) {
			num, err := strconv.Atoi(n)
			if err != nil {
				panic("cannot parse number")
			}
			if _, ok := winning[num]; ok {
				if score == 0 {
					score++
				} else {
					score *= 2
				}
			}
		}

		sum += score

	}
	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}
