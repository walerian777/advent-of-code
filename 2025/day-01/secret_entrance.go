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

func part2() int {
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
	var count, sum int

	for scanner.Scan() {
		l := scanner.Text()
		dir := l[0]
		val, err := strconv.Atoi(l[1:])
		if err != nil {
			panic("cannot convert to int")
		}
		count, dial = rotateAndCount(dir, dial, val)
		sum += count
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}

func rotateAndCount(dir byte, dial, val int) (int, int) {
	var count int
	if dir == 'R' {
		newDial := dial + val
		if newDial >= 100 {
			count += newDial / 100
		}
		if newDial == 0 {
			count += 1
		}
		return count, newDial % 100
	} else {
		if dial == 0 {
			dial = 100
		}
		newDial := dial - val
		if newDial == 0 {
			count += 1
		}
		if newDial < 0 {
			div := newDial / -100
			if newDial+div*100 < 0 {
				div += 1
			}
			count += div
			adjustedDial := newDial + div*100
			if adjustedDial == 0 {
				count += 1
			}
			return count, adjustedDial
		} else {
			return count, newDial
		}
	}
}

func rotate(dir byte, dial, val int) int {
	if dir == 'R' {
		return (dial + val) % 100
	} else {
		x := dial - val
		if x < 0 {
			div := x / -100
			return x + div*100
		} else {
			return x
		}
	}
}
