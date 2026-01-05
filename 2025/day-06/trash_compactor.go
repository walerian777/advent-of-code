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
	var sheet [][]int
	var ops []byte

	for scanner.Scan() {
		l := strings.Split(scanner.Text(), " ")
		if l[0] == "*" || l[0] == "+" {
			for _, v := range l {
				if v == "" {
					continue
				}
				ops = append(ops, v[0])
			}
		} else {
			var row []int
			for _, v := range l {
				if v == "" {
					continue
				}
				num := toIntOrPanic(v)
				row = append(row, num)
			}
			sheet = append(sheet, row)
		}
	}

	t := transpose(sheet)

	var total int
	for i, op := range ops {
		if op == '*' {
			total += mult(t[i])
		} else if op == '+' {
			total += sum(t[i])
		} else {
			panic("unknown op")
		}
	}

	return total
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
	var sheet [][]byte
	var ops []byte

	for scanner.Scan() {
		l := scanner.Text()
		if l[0] == '*' || l[0] == '+' {
			s := strings.Split(l, " ")
			for _, v := range s {
				if v == "" {
					continue
				}
				ops = append(ops, v[0])
			}
		} else {
			sheet = append(sheet, []byte(l))
		}
	}

	t := octoTranspose(sheet)
	slices.Reverse(ops)

	var total int
	for i, op := range ops {
		if op == '*' {
			total += mult(t[i])
		} else if op == '+' {
			total += sum(t[i])
		} else {
			panic("unknown op")
		}
	}

	return total
}

func transpose(table [][]int) [][]int {
	var res [][]int
	for i := 0; i < len(table[0]); i++ {
		var row []int
		for j := 0; j < len(table); j++ {
			row = append(row, table[j][i])
		}
		res = append(res, row)
	}
	return res
}

func octoTranspose(table [][]byte) [][]int {
	var res [][]int
	var row []int
	for i := len(table[0]) - 1; i >= 0; i-- {
		var nums []byte
		for j := 0; j < len(table); j++ {
			nums = append(nums, table[j][i])
		}
		trimmed := strings.TrimSpace(string(nums))
		if trimmed == "" {
			res = append(res, row)
			row = make([]int, 0)
		} else {
			n := toIntOrPanic(trimmed)
			row = append(row, n)
		}
	}
	res = append(res, row)

	return res
}

func sum(xs []int) int {
	var res int
	for _, x := range xs {
		res += x
	}
	return res
}

func mult(xs []int) int {
	res := 1
	for _, x := range xs {
		res *= x
	}
	return res
}

func toIntOrPanic(s string) int {
	num, err := strconv.Atoi(s)
	if err != nil {
		panic(fmt.Sprintf("cannot conv to int: `%v`", s))
	}

	return num
}
