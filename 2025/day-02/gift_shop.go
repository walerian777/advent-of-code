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
	First int
	Last  int
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

	var sum int

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	line := scanner.Text()

	if scanner.Err() != nil {
		panic("scanner error")
	}

	for _, s := range strings.Split(line, ",") {
		r := toRange(s)
		for _, id := range r.All() {
			if isInvalid(id) {
				sum += id
			}
		}
	}

	return sum
}

func (r *Range) All() []int {
	var ids []int
	for i := r.First; i <= r.Last; i++ {
		ids = append(ids, i)
	}
	return ids
}

func toRange(text string) Range {
	ids := strings.Split(text, "-")
	first, err := strconv.Atoi(ids[0])
	if err != nil {
		panic("cannot convert to int")
	}
	last, err := strconv.Atoi(ids[1])
	if err != nil {
		panic("cannot convert to int")
	}
	return Range{first, last}
}

func isInvalid(id int) bool {
	strId := strconv.Itoa(id)
	l := len(strId)
	if l%2 != 0 {
		return false
	}

	i := 0
	j := l / 2
	for i < l/2 {
		if strId[i] != strId[j] {
			return false
		}
		i++
		j++
	}

	return true
}
