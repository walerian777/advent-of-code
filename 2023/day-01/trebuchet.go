package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

var alphaDigits = [...]string{
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
}

var alphaDigitsReversed = [...]string{
	"eno",
	"owt",
	"eerht",
	"ruof",
	"evif",
	"xis",
	"neves",
	"thgie",
	"enin",
}

func main() {
	fmt.Println(trebuchet())

}

func trebuchet() int {
	file, err := os.Open("input")
	if err != nil {
		fmt.Println(err)
		panic("cannot read input")
	}
	defer file.Close()

	sum := 0
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()

	forward:
		for i := 0; i < len(line); i++ {
			val, err := strconv.Atoi(string(line[i]))
			if err == nil {
				sum += val * 10
				break
			}
			for j, d := range alphaDigits {
				if d[0] != line[i] {
					continue
				}
				for k := 0; k < len(d); k++ {
					if i+k >= len(line) || d[k] != line[i+k] {
						break
					}
					if k+1 == len(d) {
						sum += (j + 1) * 10
						break forward
					}
				}
			}
		}
	backward:
		for i := len(line) - 1; i >= 0; i-- {
			val, err := strconv.Atoi(string(line[i]))
			if err == nil {
				sum += val
				break
			}
			for j, d := range alphaDigitsReversed {
				if d[0] != line[i] {
					continue
				}
				for k := 0; k < len(d); k++ {
					if i-k <= 0 || d[k] != line[i-k] {
						break
					}
					if k+1 == len(d) {
						sum += (j + 1)
						break backward
					}
				}
			}
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Println(err)
		panic("scanner error")
	}

	return sum
}
