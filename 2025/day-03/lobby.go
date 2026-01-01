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

	defer func() {
		err := file.Close()
		if err != nil {
			panic("cannot close file")
		}
	}()

	scanner := bufio.NewScanner(file)
	var fst, snd byte
	var sum int

	for scanner.Scan() {
		l := scanner.Text()
		fst, snd = l[0], l[1]
		for i := 1; i < len(l)-1; i++ {
			c := l[i]
			d := l[i+1]
			if c > fst {
				fst = c
				snd = d
			} else if d > snd {
				snd = d
			}
		}
		sum += (int(fst)-48)*10 + (int(snd) - 48)
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
	var sum int

	for scanner.Scan() {
		l := scanner.Text()
		bank := make([]byte, 0, 12)
		i := 0

		for space := 12; space > 0; space-- {
			var val byte
			var valIdx int

			for j := i; j < len(l)-space+1; j++ {
				if l[j] > val {
					val = l[j]
					valIdx = j - i
				}
			}

			bank = append(bank, val)
			i += valIdx + 1
		}

		n := 0
		for _, val := range bank {
			n = n*10 + (int(val) - 48)
		}

		sum += n
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}
