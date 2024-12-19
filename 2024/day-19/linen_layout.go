package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

var cache = make(map[string]bool)

func IsSubDesignPossible(sub string, patterns *[]string) bool {
	if res, ok := cache[sub]; ok {
		return res
	}
	for _, p := range *patterns {
		if sub == p {
			cache[sub] = true
			return true
		}
		if len(p) <= len(sub) && sub[:len(p)] == p && IsSubDesignPossible(sub[len(p):], patterns) {
			cache[sub] = true
			return true
		}
	}
	cache[sub] = false
	return false
}

var sumCache = make(map[string]int)

func SumDesigns(sub string, patterns *[]string) int {
	if res, ok := sumCache[sub]; ok {
		return res
	}
	var sum int
	for _, p := range *patterns {
		if sub == p {
			sum += 1
		} else if len(p) <= len(sub) && sub[:len(p)] == p {
			sum += SumDesigns(sub[len(p):], patterns)
		}
	}
	sumCache[sub] = sum
	return sum
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	patterns := strings.Split(scanner.Text(), ", ")

	var designs []string
	for scanner.Scan() {
		designs = append(designs, scanner.Text())
	}

	var possible int
	for _, design := range designs {
		if IsSubDesignPossible(design, &patterns) {
			possible++
		}
	}

	return possible
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	patterns := strings.Split(scanner.Text(), ", ")

	var designs []string
	for scanner.Scan() {
		designs = append(designs, scanner.Text())
	}

	var sum int
	for _, design := range designs {
		sum += SumDesigns(design, &patterns)
	}

	return sum
}
