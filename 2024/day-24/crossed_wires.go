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
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	wires := make(map[string]int)
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			break
		}
		wires[l[:3]] = MustAtoi(l[5:6])
	}

	var ops []string
	for scanner.Scan() {
		ops = append(ops, scanner.Text())
	}
	var done int
	for done < len(ops) {
		for _, l := range ops {
			switch l[4] {
			case 'A':
				in1, in2, out := l[0:3], l[8:11], l[15:18]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] & wires[in2]
				done++
			case 'O':
				in1, in2, out := l[0:3], l[7:10], l[14:17]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] | wires[in2]
				done++
			case 'X':
				in1, in2, out := l[0:3], l[8:11], l[15:18]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] ^ wires[in2]
				done++
			}
		}
	}
	var z int
	for i := range 46 {
		var k string
		if i < 10 {
			k = fmt.Sprintf("z0%d", i)
		} else {
			k = fmt.Sprintf("z%d", i)
		}
		z += wires[k] << i
	}

	return z
}

func part2() int {
	return 0
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}
