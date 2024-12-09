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
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	input := scanner.Text()

	disk := make(map[int]int)
	empty := make(map[int]int)
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
				empty[idx] = empty[idx-1] + 1
				idx++
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	for i := len(disk) - 1; i >= 0; i-- {
		c := disk[i]
		cap := 1
		if c > -1 {
			for i >= 0 {
				if disk[i-1] == c {
					i--
					cap++
				} else {
					break
				}
			}
			for j := 0; j < i; j++ {
				if empty[j] == cap {
					l := 0
					for k := j - cap + 1; k <= j; k++ {
						disk[i+l] = -1
						empty[i+l] = 1
						l++
						disk[k] = c
						delete(empty, k)
					}
					for k := j + 1; k < len(disk); k++ {
						if empty[k] > 0 {
							empty[k] -= cap
						} else {
							break
						}
					}

					break
				}
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
