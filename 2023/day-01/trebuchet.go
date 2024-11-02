package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
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
		calibration := 0

		for i := 0; i < len(line); i++ {
			val, err := strconv.Atoi(string(line[i]))
			if err == nil {
				calibration += val * 10
				break
			}
		}
		for i := len(line) - 1; i >= 0; i-- {
			val, err := strconv.Atoi(string(line[i]))
			if err == nil {
				calibration += val
				break
			}
		}
		sum += calibration
	}
	if err := scanner.Err(); err != nil {
		fmt.Println(err)
		panic("scanner error")
	}

	fmt.Println(sum)
}
