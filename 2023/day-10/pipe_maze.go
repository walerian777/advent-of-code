package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strings"
)

func main() {
	fmt.Println(countSteps())
	fmt.Println(tilesEnclosedByLoop())
}

type Coord struct {
	x, y int
}

func NewCoord(x, y int) *Coord {
	return &Coord{x: x, y: y}
}

func (c *Coord) String() string {
	return fmt.Sprintf("(%d, %d)", c.x, c.y)
}

func countSteps() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var maze []string
	var startX, startY int
	var startFound bool
	for scanner.Scan() {
		line := scanner.Text()
		if !startFound {
			idx := strings.IndexByte(line, 'S')
			if idx != -1 {
				startY = idx
				startFound = true
			} else {
				startX++
			}
		}
		maze = append(maze, line)
	}
	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	root := NewCoord(startX, startY)
	queue := []*Coord{root}
	steps := make(map[Coord]int)
	steps[*root] = 0
	var maxSteps int

	for len(queue) > 0 {
		coord := queue[0]
		queue = queue[1:]

		ns := neighbors(coord)
		for dir, n := range ns {
			if steps[*n] > 0 {
				continue
			}
			if validPipe(dir, maze[n.x][n.y]) {
				steps[*n] = steps[*coord] + 1
				if steps[*n] > maxSteps {
					maxSteps = steps[*n]
				}
				queue = append(queue, n)
			}
		}
	}
	return maxSteps
}

func tilesEnclosedByLoop() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var maze []string
	var startX, startY int
	var startFound bool
	for scanner.Scan() {
		line := scanner.Text()
		if !startFound {
			idx := strings.IndexByte(line, 'S')
			if idx != -1 {
				startY = idx
				startFound = true
			} else {
				startX++
			}
		}
		maze = append(maze, line)
	}
	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	root := NewCoord(startX, startY)
	queue := []*Coord{root}
	steps := make(map[Coord]int)
	steps[*root] = 0
	var maxSteps int

	for len(queue) > 0 {
		coord := queue[0]
		queue = queue[1:]

		ns := neighbors(coord)
		for dir, n := range ns {
			if steps[*n] > 0 {
				continue
			}
			if validPipe(dir, maze[n.x][n.y]) {
				steps[*n] = steps[*coord] + 1
				if steps[*n] > maxSteps {
					maxSteps = steps[*n]
				}
				queue = append(queue, n)
			}
		}
	}
	var cs []Coord
	for c := range steps {
		cs = append(cs, c)
	}
	slices.SortFunc(cs, func(a, b Coord) int {
		if a.x < b.x {
			return -1
		} else if a.x > b.x {
			return 1
		}
		if a.y < b.y {
			return -1
		} else if a.y > b.y {
			return 1
		}
		return 0
	})
	slices.Compact(cs)
	return (shoelace(cs) - (maxSteps * 2) + 3) / 2
}

func shoelace(cs []Coord) int {
	var sum int
	for i := 1; i <= len(cs); i++ {
		c1 := cs[i-1]
		c2 := cs[0]
		if i < len(cs) {
			c2 = cs[i]
		}
		sum += ((c1.x * c2.y) - (c1.y * c2.x))
	}
	if sum < 0 {
		sum = -sum
	}
	return sum
}

func neighbors(c *Coord) map[byte]*Coord {
	ns := make(map[byte]*Coord)
	// north
	if c.x > 0 {
		ns['n'] = NewCoord(c.x-1, c.y)
	}
	// east
	if c.y < 139 {
		ns['e'] = NewCoord(c.x, c.y+1)
	}
	// south
	if c.x < 139 {
		ns['s'] = NewCoord(c.x+1, c.y)
	}
	// west
	if c.y > 0 {
		ns['w'] = NewCoord(c.x, c.y-1)

	}
	return ns
}

func validPipe(dir, tile byte) bool {
	switch dir {
	case 'n':
		return tile == '|' || tile == '7' || tile == 'F'
	case 'e':
		return tile == '-' || tile == '7' || tile == 'J'
	case 's':
		return tile == '|' || tile == 'L' || tile == 'J'
	case 'w':
		return tile == '-' || tile == 'L' || tile == 'F'
	default:
		panic("invalid direction")
	}
}
