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
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var l1, l2 []int

	for scanner.Scan() {
		line := strings.Fields(scanner.Text())
		n, err := strconv.Atoi(line[0])
		if err != nil {
			panic("cannot conver to int")
		}
		l1 = append(l1, n)
		n, err = strconv.Atoi(line[1])
		if err != nil {
			panic("cannot conver to int")
		}
		l2 = append(l2, n)

	}
	slices.Sort(l1)
	slices.Sort(l2)

	var sum int
	for i, a := range l1 {
		x := a - l2[i]
		if x < 0 {
			x = -x
		}
		sum += x

	}
	return sum
}