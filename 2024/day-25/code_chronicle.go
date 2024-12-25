package main

import (
	"bufio"
	"fmt"
	"os"
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

	var locks, keys [][5]int
	var current [5]int
	var isLock bool
	var i int
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			i = 0
			continue
		}
		if i == 0 {
			isLock = l[0] == '#'
			i++
			continue
		}
		if i == 6 {
			if isLock {
				locks = append(locks, current)
			} else {
				keys = append(keys, current)
			}
			for j := range current {
				current[j] = 0
			}
			continue
		}
		for j := range l {
			if l[j] == '#' {
				current[j]++
			}
		}
		i++
	}

	var sum int
	for _, k := range keys {
	lockloop:
		for _, l := range locks {
			for i := range k {
				if k[i]+l[i] > 5 {
					continue lockloop
				}
			}
			sum++
		}
	}

	return sum
}
