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

	scanner.Scan()
	steps := strings.Split(scanner.Text(), ",")

	var sum int
	for _, s := range steps {
		sum += hash(s)
	}

	return sum
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	steps := strings.Split(scanner.Text(), ",")

	hashes := make(map[string]int)
	boxes := make(map[int]map[string][2]int)

	for _, s := range steps {
		if eqIdx := strings.Index(s, "="); eqIdx > -1 {
			label := s[:eqIdx]
			lens := MustAtoi(s[eqIdx+1:])
			h := hashMem(label, &hashes)

			if _, ok := boxes[h]; !ok {
				boxes[h] = make(map[string][2]int)
			}
			b := boxes[h]
			if l, ok := b[label]; ok {
				b[label] = [2]int{l[0], lens}
			} else {
				b[label] = [2]int{len(b) + 1, lens}
			}
		} else {
			label := s[:len(s)-1]
			h := hashMem(label, &hashes)

			if _, ok := boxes[h]; !ok {
				boxes[h] = make(map[string][2]int)
				continue
			}
			b := boxes[h]
			if _, ok := b[label]; !ok {
				continue
			}
			pos := b[label][0]
			delete(b, label)
			for k, v := range b {
				if v[0] < pos {
					continue
				}
				b[k] = [2]int{v[0] - 1, v[1]}
			}
		}
	}

	var sum int
	for b, lenses := range boxes {
		for _, v := range lenses {
			sum += (b+1) * v[0] * v[1]
		}
	}

	return sum
}

func hash(s string) int {
	var h int
	for i := range s {
		h = ((h + int(s[i])) * 17) & 0b11111111
	}

	return h
}

func hashMem(s string, mem *map[string]int) int {
	if v, ok := (*mem)[s]; ok {
		return v
	}
	var h int
	for i := range s {
		h = ((h + int(s[i])) * 17) & 0b11111111
	}
	(*mem)[s] = h

	return h
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}
