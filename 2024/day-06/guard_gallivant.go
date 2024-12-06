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
	dir  byte
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var grid [][]byte
	currPos := &Coord{-1, -1, 'N'}

	for scanner.Scan() {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		if currPos.x > -1 {
			continue
		}
		if y := slices.Index(line, '^'); y > -1 {
			currPos.x = len(grid) - 1
			currPos.y = y
		}
	}

	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1
	visited := make(map[int]bool)
	visited[currPos.LinearIndex()] = true

	for {
		newX, newY := currPos.NextCoord()
		if newX < 0 || newY < 0 || newX > maxX || newY > maxY {
			break
		}
		if grid[newX][newY] == '#' {
			currPos.dir = currPos.NextDir()
		} else {
			currPos.x = newX
			currPos.y = newY
			visited[currPos.LinearIndex()] = true
		}
	}

	return len(visited)
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var grid [][]byte
	startPos := &Coord{-1, -1, 'N'}

	for scanner.Scan() {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		if startPos.x != -1 {
			continue
		}
		if y := slices.Index(line, '^'); y > -1 {
			startPos.x = len(grid) - 1
			startPos.y = y
		}
	}

	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1
	var sum int

	for obsX := 0; obsX <= maxX; obsX++ {
		for obsY := 0; obsY <= maxY; obsY++ {
			if grid[obsX][obsY] != '.' {
				continue
			}

			currPos := &Coord{startPos.x, startPos.y, startPos.dir}
			visited := make(map[int]byte)
			visited[currPos.LinearIndex()] = currPos.dir
			for {
				newX, newY := currPos.NextCoord()
				if newX < 0 || newY < 0 || newX > maxX || newY > maxY {
					break
				}
				if (newX == obsX && newY == obsY) || grid[newX][newY] == '#' {
					currPos.dir = currPos.NextDir()
				} else if d := visited[linearIndex(newX, newY)]; d == currPos.dir {
					sum++
					break
				} else {
					currPos.x = newX
					currPos.y = newY
					visited[currPos.LinearIndex()] = currPos.dir
				}
			}
		}
	}

	return sum
}

func (c *Coord) NextCoord() (int, int) {
	switch c.dir {
	case 'N':
		return c.x - 1, c.y
	case 'E':
		return c.x, c.y + 1
	case 'S':
		return c.x + 1, c.y
	case 'W':
		return c.x, c.y - 1
	default:
		panic("invalid direction")
	}
}

func (c *Coord) NextDir() byte {
	switch c.dir {
	case 'N':
		return 'E'
	case 'E':
		return 'S'
	case 'S':
		return 'W'
	case 'W':
		return 'N'
	default:
		panic("invalid direction")
	}
}

func (c *Coord) LinearIndex() int {
	return linearIndex(c.x, c.y)
}

func linearIndex(x, y int) int {
	return y*1000 + x
}
