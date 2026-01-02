package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
}

type Range struct {
	Start, End int
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}

	defer func() {
		if err := file.Close(); err != nil {
			panic("cannot close file")
		}
	}()

	scanner := bufio.NewScanner(file)
	var fresh int

	var ranges []Range
	readingRanges := true

	for scanner.Scan() {
		l := scanner.Text()

		if l == "" {
			readingRanges = false
			continue
		}

		if readingRanges {
			fst, snd, fnd := strings.Cut(l, "-")
			if !fnd {
				panic("did not found `-`")
			}
			ranges = append(ranges, Range{toIntOrPanic(fst), toIntOrPanic(snd)})
		} else {
			id := toIntOrPanic(l)
			for _, r := range ranges {
				if id >= r.Start && id <= r.End {
					fresh++
					break
				}
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	return fresh
}

func toIntOrPanic(s string) int {
	num, err := strconv.Atoi(s)
	if err != nil {
		panic("cannot conv to int")
	}

	return num
}
