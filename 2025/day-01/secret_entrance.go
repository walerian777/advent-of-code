package main


import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)


func main() {
	fmt.Println(part1())
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}

	defer func() {
		err := file.Close()
		if err != nil {
			panic("cannot close file")
		}
	}()

	scanner := bufio.NewScanner(file)
	dial := 50
	sum := 0

	for scanner.Scan() {
		l := scanner.Text()
		dir := l[0]
		val, err := strconv.Atoi(l[1:])
		if err != nil {
			panic("cannot convert to int")
		}
		dial = rotate(dir, dial, val)
		if dial == 0 {
			sum += 1
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}

func rotate(dir byte, dial, val int) int {
	if dir == 'R' {
		return (dial + val) % 100
	} else {
		x := dial - val
		if x < 0 {
			div := x / -100
			return x + div * 100
		} else {
			return x
		}
	}
}
