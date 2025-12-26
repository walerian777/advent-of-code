package main

import (
	"bufio"
	"fmt"
	"os"
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
	var fst, snd byte
	var sum int

	for scanner.Scan() {
		l := scanner.Text()
		fst, snd = l[0], l[1]
		for i := 1; i < len(l) - 1; i++ {
			c := l[i]
			d := l[i+1]
			if c > fst {
				fst = c
				snd = d
			} else if d > snd {
				snd = d
			}
		}
		sum += (int(fst) - 48) * 10 + (int(snd) - 48)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return sum
}
