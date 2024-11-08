package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(possibleGames())
	fmt.Println(sumOfPowers())
}

func possibleGames() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	re := regexp.MustCompile(`\d+\s(red|blue|green)`)
	sum := 0
	i := 1

	for scanner.Scan() {
		line := scanner.Text()
		isPossible := true
		sets := strings.Split(line, "; ")
	outer:
		for _, set := range sets {
			cubes := re.FindAllString(set, 3)
			for _, cube := range cubes {
				parts := strings.SplitN(cube, " ", 2)
				count, err := strconv.Atoi(parts[0])
				if err != nil {
					panic("expected a number, got something else")
				}
				if parts[1] == "red" {
					if count > 12 {
						isPossible = false
						break outer
					}
				} else if parts[1] == "green" {
					if count > 13 {
						isPossible = false
						break outer
					}
				} else if parts[1] == "blue" {
					if count > 14 {
						isPossible = false
						break outer
					}
				} else {
					panic("expected red/green/blue, got something else")
				}
			}
		}
		if isPossible {
			sum += i
		}
		i += 1
	}

	return sum
}

func sumOfPowers() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	sum := 0
	re := regexp.MustCompile(`\d+\s(red|blue|green)`)

	for scanner.Scan() {
		line := scanner.Text()
		sets := strings.Split(line, "; ")
		var b, r, g int
		for _, set := range sets {
			cubes := re.FindAllString(set, 3)
			for _, cube := range cubes {
				parts := strings.SplitN(cube, " ", 2)
				count, err := strconv.Atoi(parts[0])
				if err != nil {
					panic("expected a number, got something else")
				}
				if parts[1] == "red" {
					if count > r {
						r = count
					}
				} else if parts[1] == "green" {
					if count > g {
						g = count
					}
				} else if parts[1] == "blue" {
					if count > b {
						b = count
					}
				} else {
					panic("expected red/green/blue, got something else")
				}
			}
		}
		sum += r * b * g
	}

	return sum
}
