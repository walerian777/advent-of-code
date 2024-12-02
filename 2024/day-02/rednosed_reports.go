package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var safe int
	for scanner.Scan() {
		line := strings.Fields(scanner.Text())
		if evalLine(line) {
			safe++
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return safe
}

func evalLine(line []string) bool {
	p, err := strconv.Atoi(line[0])
	if err != nil {
		panic("cannot convert to int")
	}
	dir := "u"
	for i := 1; i < len(line); i++ {
		n, err := strconv.Atoi(line[i])
		if err != nil {
			panic("cannot convert to int")
		}
		ab := abs(p - n)
		if ab > 3 || ab < 1 {
			return false
		}
		if dir == "u" {
			if n > p {
				dir = "i"
			} else {
				dir = "d"
			}
		} else if dir == "i" {
			if n <= p {
				return false
			}
		} else if dir == "d" {
			if n >= p {
				return false
			}
		}
		p = n
	}
	return true
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var safe int
	for scanner.Scan() {
		line := strings.Fields(scanner.Text())
		for i := 0; i < len(line); i++ {
			subLine := []string{}
			subLine = append(subLine, line[0:i]...)
			subLine = append(subLine, line[i+1:]...)
			if evalLine(subLine) {
				safe++
				break
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return safe
}
