package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

type Coord struct {
	x, y int
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var grid [][]byte
	var robot *Coord
	var robotFound bool
	for i := 0; scanner.Scan(); i++ {
		line := []byte(scanner.Text())
		if len(line) == 0 {
			break
		}
		grid = append(grid, line)
		if !robotFound {
			if j := slices.Index(line, '@'); j > -1 {
				robot = &Coord{i, j}
				robotFound = true
			}
		}

	}

	var moves []byte
	for scanner.Scan() {
		line := scanner.Text()
		moves = append(moves, line...)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	for _, move := range moves {
		var steps int
		x := robot.x
		y := robot.y

		switch move {
		case '^':
			x--
			for grid[x][y] == 'O' {
				steps++
				x--
			}
			if grid[x][y] == '#' {
				continue
			}
			for steps > 0 {
				grid[x][y] = 'O'
				x++
				steps--
			}
			grid[x][y] = '@'
			grid[x+1][y] = '.'
			robot.x--
		case '>':
			y++
			for grid[x][y] == 'O' {
				steps++
				y++
			}
			if grid[x][y] == '#' {
				continue
			}
			for steps > 0 {
				grid[x][y] = 'O'
				y--
				steps--
			}
			grid[x][y] = '@'
			grid[x][y-1] = '.'
			robot.y++
		case '<':
			y--
			for grid[x][y] == 'O' {
				steps++
				y--
			}
			if grid[x][y] == '#' {
				continue
			}
			for steps > 0 {
				grid[x][y] = 'O'
				y++
				steps--
			}
			grid[x][y] = '@'
			grid[x][y+1] = '.'
			robot.y--
		case 'v':
			x++
			for grid[x][y] == 'O' {
				steps++
				x++
			}
			if grid[x][y] == '#' {
				continue
			}
			for steps > 0 {
				grid[x][y] = 'O'
				x--
				steps--
			}
			grid[x][y] = '@'
			grid[x-1][y] = '.'
			robot.x++
		default:
			panic("unknown move")
		}
	}

	var sum int
	for i := range grid {
		for j := range grid[i] {
			if grid[i][j] == 'O' {
				sum += Gps(i, j)
			}
		}
	}

	return sum
}

func part2() int {
	return 0
}

func Gps(x, y int) int {
	return 100*x + y
}
