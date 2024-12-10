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

type Coord struct {
	x, y int
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var grid [][]int
	var trailheads []Coord
	for scanner.Scan() {
		line := scanner.Text()
		var ns []int
		for _, c := range line {
			n, err := strconv.Atoi(string(c))
			if err != nil {
				panic("strconv")
			}
			if n == 0 {
				trailheads = append(trailheads, Coord{len(grid), len(ns)})
			}
			ns = append(ns, n)
		}
		grid = append(grid, ns)
	}

	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1

	var sum int
	for _, th := range trailheads {
		q := []Coord{th}
		peaks := make(map[Coord]bool)
		for len(q) > 0 {
			c := q[0]
			q = q[1:]
			ns := neighbors(&c, maxX, maxY)
			for _, n := range ns {
				if grid[n.x][n.y]-grid[c.x][c.y] == 1 {
					if grid[n.x][n.y] == 9 {
						peaks[n] = true
					} else {
						q = append(q, Coord{n.x, n.y})
					}
				}
			}
		}
		sum += len(peaks)
	}

	return sum
}

func neighbors(c *Coord, maxX, maxY int) []Coord {
	var ns []Coord
	// north
	if c.x > 0 {
		ns = append(ns, Coord{c.x - 1, c.y})
	}
	// east
	if c.y < maxX {
		ns = append(ns, Coord{c.x, c.y + 1})
	}
	// south
	if c.x < maxY {
		ns = append(ns, Coord{c.x + 1, c.y})
	}
	// west
	if c.y > 0 {
		ns = append(ns, Coord{c.x, c.y - 1})

	}
	return ns
}

func part2() int {
	return 0
}
