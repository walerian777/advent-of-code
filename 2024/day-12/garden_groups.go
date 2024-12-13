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

type Garden struct {
	x, y int
}

func (g *Garden) String() string {
	return fmt.Sprintf("(%d, %d)", g.x, g.y)
}

type Region struct {
	plant   byte
	gardens map[Garden]bool
}

func (r *Region) String() string {
	return fmt.Sprintf("[%v]", string(r.plant))
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var grid [][]byte
	gardens := make(map[Garden]*Region)
	for i := 0; scanner.Scan(); i++ {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		for j, c := range line {
			g := Garden{i, j}
			r := &Region{c, map[Garden]bool{g: true}}
			gardens[g] = r
		}
	}

	maxX, maxY := len(grid)-1, len(grid[0])-1
	for i, l := range grid {
		for j := range l {
			g := Garden{i, j}
			for _, n := range neighbors(&g, maxX, maxY) {
				if grid[g.x][g.y] == grid[n.x][n.y] {
					for k := range gardens[n].gardens {
						gardens[g].gardens[k] = true
					}
					for k := range gardens[g].gardens {
						gardens[k] = gardens[g]
					}
				}

			}

		}
	}

	areas := make(map[*Region]int)
	perimeters := make(map[*Region]int)

	for k, v := range gardens {
		areas[v]++
		walls := 4
		for _, n := range neighbors(&k, maxX, maxY) {
			if r, ok := gardens[n]; ok && r.plant == v.plant {
				walls--
			}
		}
		perimeters[v] += walls
	}

	var sum int
	for k, v := range areas {
		sum += v * perimeters[k]
	}
	return sum
}

func part2() int {
	return 0
}

func neighbors(c *Garden, maxX, maxY int) []Garden {
	var ns []Garden
	// north
	if c.x > 0 {
		ns = append(ns, Garden{x: c.x - 1, y: c.y})
	}
	// east
	if c.y < maxY {
		ns = append(ns, Garden{x: c.x, y: c.y + 1})
	}
	// south
	if c.x < maxX {
		ns = append(ns, Garden{x: c.x + 1, y: c.y})
	}
	// west
	if c.y > 0 {
		ns = append(ns, Garden{x: c.x, y: c.y - 1})

	}
	return ns
}
