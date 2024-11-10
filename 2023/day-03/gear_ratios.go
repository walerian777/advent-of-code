package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	fmt.Println(sumOfParts())
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
