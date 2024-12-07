package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
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
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var sum int

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.SplitN(line, ": ", 2)
		test, err := strconv.Atoi(parts[0])
		if err != nil {
			panic("cannot parse int")
		}
		rest := strings.Fields(parts[1])
		var nums []int
		for _, s := range rest {
			n, err := strconv.Atoi(s)
			if err != nil {
				panic("cannot parse int")
			}
			nums = append(nums, n)
		}
		vs := []int{nums[0]}
		for _, x := range nums[1:] {
			var newVs []int
			for _, v := range vs {
				newVs = append(newVs, v+x, v*x)
			}
			vs = newVs
		}
		if slices.Index(vs, test) > -1 {
			sum += test
		}
	}
	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}
	return sum
}

func part2() int {
	return 0
}
