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

type Coord struct {
	X, Y int
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
	var diagram []string
	var rolls int

	for scanner.Scan() {
		l := scanner.Text()
		diagram = append(diagram, l)
	}

	rows := len(diagram)
	cols := len(diagram[0])
	neighbors := neighborsFuncGenerator(rows, cols)
	for x := 0; x < rows; x++ {
		for y := 0; y < cols; y++ {
			if diagram[x][y] != '@' {
				continue
			}
			cnt := 0
			ns := neighbors(x, y)
			for _, n := range ns {
				if diagram[n.X][n.Y] == '@' {
					cnt++
					if cnt >= 4 {
						break
					}
				}
			}
			if cnt < 4 {
				rolls++
			}
		}
	}

	return rolls
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
	var diagram [][]byte
	var rolls int

	for scanner.Scan() {
		l := scanner.Text()
		diagram = append(diagram, []byte(l))
	}

	rows := len(diagram)
	cols := len(diagram[0])
	neighbors := neighborsFuncGenerator(rows, cols)

	for {
		removed := 0
		for x := 0; x < rows; x++ {
			for y := 0; y < cols; y++ {
				if diagram[x][y] != '@' {
					continue
				}
				cnt := 0
				ns := neighbors(x, y)
				for _, n := range ns {
					if diagram[n.X][n.Y] == '@' {
						cnt++
						if cnt >= 4 {
							break
						}
					}
				}
				if cnt < 4 {
					diagram[x][y] = 'x'
					rolls++
					removed++
				}
			}
		}
		if removed == 0 {
			break
		}
	}

	return rolls
}

func neighborsFuncGenerator(maxX, maxY int) func(int, int) []Coord {
	return func(x, y int) []Coord {
		return neighborsWithLimits(x, y, maxX, maxY)
	}
}

func neighborsWithLimits(x, y, maxX, maxY int) []Coord {
	deltas := []Coord{
		{-1, -1}, {-1, 0}, {-1, 1},
		{0, -1}, {0, 1},
		{1, -1}, {1, 0}, {1, 1},
	}
	var valid []Coord
	for _, d := range deltas {
		newX, newY := x+d.X, y+d.Y
		if newX < 0 || newX >= maxX {
			continue
		}
		if newY < 0 || newY >= maxY {
			continue
		}
		valid = append(valid, Coord{newX, newY})
	}

	return valid
}
