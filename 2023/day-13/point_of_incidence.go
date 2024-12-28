package main

import (
	"bufio"
	"fmt"
	"os"
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
	var pattern []string
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			sum += findReflection(pattern)
			pattern = pattern[0:0]
			continue
		}
		pattern = append(pattern, l)
	}
	sum += findReflection(pattern)

	return sum
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var sum int
	var pattern []string
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			sum += findReflectionWithSmudge(pattern)
			pattern = pattern[0:0]
			continue
		}
		pattern = append(pattern, l)
	}
	sum += findReflectionWithSmudge(pattern)

	return sum
}

func findReflection(pattern []string) int {
rows:
	for i := 1; i < len(pattern); i++ {
		for j := 0; i-j > 0 && i+j < len(pattern); j++ {
			if pattern[i-j-1] != pattern[i+j] {
				continue rows
			}
		}
		return 100 * i
	}
	columns := transpose(pattern)
cols:
	for i := 1; i < len(columns); i++ {
		for j := 0; i-j > 0 && i+j < len(columns); j++ {
			if columns[i-j-1] != columns[i+j] {
				continue cols
			}
		}
		return i
	}
	panic("not found")
}

func transpose(rows []string) []string {
	cols := make([][]byte, len(rows[0]))
	for i := range rows {
		for j := range rows[i] {
			if i == 0 {
				cols[j] = make([]byte, len(rows))
			}
			cols[j][i] = rows[i][j]
		}
	}

	var out []string
	for _, c := range cols {
		out = append(out, string(c))
	}

	return out
}

func findReflectionWithSmudge(pattern []string) int {
rows:
	for i := 1; i < len(pattern); i++ {
		var hasSmudge bool
		for j := 0; i-j > 0 && i+j < len(pattern); j++ {
			if pattern[i-j-1] != pattern[i+j] {
				if !hasSmudge && isSmudge(pattern[i-j-1], pattern[i+j]) {
					hasSmudge = true
				} else {
					continue rows
				}
			}
		}
		if !hasSmudge {
			continue
		}
		return 100 * i
	}
	columns := transpose(pattern)
cols:
	for i := 1; i < len(columns); i++ {
		var hasSmudge bool
		for j := 0; i-j > 0 && i+j < len(columns); j++ {
			if columns[i-j-1] != columns[i+j] {
				if !hasSmudge && isSmudge(columns[i-j-1], columns[i+j]) {
					hasSmudge = true
				} else {
					continue cols
				}
			}
		}
		if !hasSmudge {
			continue
		}
		return i
	}
	panic("not found")
}

func isSmudge(s1, s2 string) bool {
	var dist int
	for i := range s1 {
		if s1[i] != s2[i] {
			dist++

			if dist > 1 {
				return false
			}
		}
	}
	return dist == 1
}
