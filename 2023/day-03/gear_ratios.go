package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	fmt.Println(sumOfParts())
	fmt.Println(sumOfGears())
}

func sumOfParts() int {
	sum := 0

	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := make([]string, 0, 140)

	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	for i, line := range lines {
		num := make([]byte, 0, 3)

		for j := 0; j < len(line); j++ {
			c := line[j]
			if c >= '0' && c <= '9' {
				num = append(num, c)
				if j < len(line)-1 {
					continue
				}
			}
			if len(num) > 0 {
				val, err := strconv.Atoi(string(num))
				if err != nil {
					panic("expected number")
				}

				iFrom := max(0, i-1)
				iTo := min(i+1, len(lines)-1)
				jFrom := max(0, j-len(num)-1)
				jTo := min(j, len(line)-1)

			outer:
				for k := iFrom; k <= iTo; k++ {
					for l := jFrom; l <= jTo; l++ {
						d := lines[k][l]
						if d != '.' && (d < '0' || d > '9') {
							sum += val
							break outer
						}
					}
				}

				num = make([]byte, 0, 3)

			}
		}
	}
	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}

type coord struct {
	x, y int
}

type part struct {
	val, startIndex, len int
}

func sumOfGears() int {
	sum := 0

	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := make([]string, 0, 140)
	parts := make(map[coord]part)
	i := 0

	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)

		num := make([]rune, 0, 3)
		startIndex := 0
		for j, c := range line {
			if c >= '0' && c <= '9' {
				if len(num) == 0 {
					startIndex = j
				}
				num = append(num, c)
				if j < len(line)-1 {
					continue
				}
			}
			if len(num) == 0 {
				continue
			}
			val, err := strconv.Atoi(string(num))
			if err != nil {
				panic("expected a number")
			}
			part := part{val, startIndex, len(num)}
			for m := 0; m < len(num); m++ {

				parts[coord{i, startIndex + m}] = part
			}
			num = make([]rune, 0, 3)
			startIndex = 0
		}
		i++
	}

	for i, line := range lines {
		for j, c := range line {
			if c != '*' {
				continue
			}

			iFrom := max(0, i-1)
			iTo := min(i+1, len(lines)-1)
			jFrom := max(0, j-1)
			jTo := min(j+1, len(line)-1)
			var p1 int

		outer:
			for k := iFrom; k <= iTo; k++ {
				for l := jFrom; l <= jTo; l++ {
					part, ok := parts[coord{k, l}]
					if !ok {
						continue
					}
					l = part.startIndex + part.len
					if p1 == 0 {
						p1 = part.val
						continue
					}
					sum += p1 * part.val
					break outer
				}
			}

		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}
