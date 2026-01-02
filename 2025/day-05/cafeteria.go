package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

type Range struct {
	Start, End int
}

func (r Range) Overlap(o Range) (Range, bool) {
	if r.Start > o.End || o.Start > r.End {
		return Range{0, 0}, false
	}
	return Range{min(r.Start, o.Start), max(o.End, r.End)}, true
}

func (r Range) Length() int {
	return r.End - r.Start + 1
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

func part2() int {
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

	var ranges []Range

	for scanner.Scan() {
		l := scanner.Text()

		if l == "" {
			break
		}

		fst, snd, fnd := strings.Cut(l, "-")
		if !fnd {
			panic("did not found `-`")
		}
		newR := Range{toIntOrPanic(fst), toIntOrPanic(snd)}
		added := false
		for i, r := range ranges {
			if ol, fnd := newR.Overlap(r); fnd {
				ranges[i] = ol
				added = true
				break
			}
		}
		if !added {
			ranges = append(ranges, newR)
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}
	stillAdding := true
	var newRanges []Range
	slices.SortFunc(ranges, func(a, b Range) int {
		return b.Start - a.Start

	})
	for stillAdding {
		stillAdding = false
		for _, oldR := range ranges {
			added := false
			for i, r := range newRanges {
				if ol, fnd := oldR.Overlap(r); fnd {
					newRanges[i] = ol
					added = true
					break
				}
			}

			if !added {
				newRanges = append(newRanges, oldR)
				stillAdding = true
			}
		}
	}

	var fresh int
	for _, r := range newRanges {
		fmt.Println(r, r.Length())
		fresh += r.Length()
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
