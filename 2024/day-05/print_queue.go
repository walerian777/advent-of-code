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
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var sum int
	orders := make(map[int][]int)
	counting := false
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			counting = true
			continue
		}
		if !counting {
			split := strings.SplitN(line, "|", 2)
			a, err := strconv.Atoi(split[0])
			if err != nil {
				panic("cannot convert to int")
			}
			b, err := strconv.Atoi(split[1])
			if err != nil {
				panic("cannot convert to int")
			}
			orders[a] = append(orders[a], b)
			continue
		}
		split := strings.Split(line, ",")
		var nums []int
		for _, x := range split {
			a, err := strconv.Atoi(x)
			if err != nil {
				panic("cannot convert to int")
			}
			nums = append(nums, a)
		}
		valid := false
		ordered := true
	orderer:
		for i, n := range nums {
			ord, ok := orders[n]
			if ok {
				valid = true
			}
			for _, m := range ord {
				j := slices.Index(nums, m)
				if j > -1 && j < i {
					ordered = false
					break orderer
				}
			}
		}
		if valid && ordered {
			sum += nums[len(nums)/2]
		}

	}

	return sum
}

func part2() int {
	return 0
}
