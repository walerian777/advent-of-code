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
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()

	var stones []int
	for _, s := range strings.Fields(scanner.Text()) {
		n, err := strconv.Atoi(s)
		if err != nil {
			panic("strconv!")
		}
		stones = append(stones, n)
	}

	for range 25 {
		var newStones []int
		for _, s := range stones {
			newS := evolve(s)
			newStones = append(newStones, newS...)
		}
		stones = newStones
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return len(stones)
}

func part2() int {
	return 0
}

func evolve(s int) []int {
	if s == 0 {
		return []int{1}
	}
	x, cnt := 10, 1
	for x <= s {
		x *= 10
		cnt++
	}
	if cnt%2 == 0 {
		return []int{s / pow(10, cnt/2), s % pow(10, cnt/2)}
	}
	return []int{s * 2024}
}

func pow(n, m int) int {
	if m == 0 {
		return 1
	}

	if m == 1 {
		return n
	}

	x := n
	for i := 2; i <= m; i++ {
		x *= n
	}
	return x
}
