package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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
	input := scanner.Text()

	disk := make(map[int]int)
	empty := make(map[int]bool)
	var id, idx int
	for i, b := range input {
		n, err := strconv.Atoi(string(b))
		if err != nil {
			panic("cannot parse int")
		}
		if i%2 == 0 {
			for range n {
				disk[idx] = id
				idx++
			}
			id++
		} else {
			for range n {
				disk[idx] = -1
				empty[idx] = true
				idx++
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var j int
	for i := len(disk) - 1; i >= 0; i-- {
		c := disk[i]
		if c > -1 {
			disk[i] = -1
			empty[i] = true
			for j < len(disk) {
				if empty[j] {
					disk[j] = c
					delete(empty, j)
					j++
					break
				}
				j++
			}
			if i < j {
				j = i - 1
			}
		}
	}

	var sum int
	for i := 0; i < len(disk); i++ {
		v := disk[i]
		if v < 0 {
			continue
		}
		sum += i * v
	}

	return sum
}

func part2() int {
	return 0
}
