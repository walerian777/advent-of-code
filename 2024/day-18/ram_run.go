package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

type Coord struct {
	x, y int
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}

func AStar(start, goal Coord, x, y int, grid *[71][71]bool) int {
	openSet := map[Coord]bool{start: true}
	gScore := map[Coord]int{start: 0}
	fScore := map[Coord]int{start: manhattanDistance(start, goal)}
	distFunc := manhattanDistance

	for len(openSet) > 0 {
		var current Coord
		minK := math.MaxInt
		for k := range openSet {
			if s, ok := fScore[k]; ok && s < minK {
				minK = fScore[k]
				current = k
			}
		}
		if current.x == goal.x && current.y == goal.y {
			return gScore[current]
		}
		delete(openSet, current)
		for _, nb := range neighbors(current) {
			if nb.x < 0 || nb.y < 0 || nb.x > x || nb.y > y {
				continue
			}
			if (*grid)[nb.x][nb.y] {
				continue
			}

			gScoreTentative := gScore[current] + distFunc(current, nb)
			if gScoreCurrent, ok := gScore[nb]; !ok || gScoreTentative < gScoreCurrent {
				gScore[nb] = gScoreTentative
				fScore[nb] = gScoreTentative + manhattanDistance(nb, goal)
				if _, ok := openSet[nb]; !ok {
					openSet[nb] = true
				}
			}
		}
	}
	panic("failed to A*")
}

func neighbors(c Coord) []Coord {
	return []Coord{
		c.plus(Coord{-1, 0}),
		c.plus(Coord{0, -1}),
		c.plus(Coord{0, 1}),
		c.plus(Coord{1, 0}),
	}
}

func (c Coord) plus(o Coord) Coord {
	return Coord{c.x + o.x, c.y + o.y}
}

func manhattanDistance(p, q Coord) int {
	return abs(p.x-q.x) + abs(p.y-q.y)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var positions []Coord
	for scanner.Scan() {
		l := strings.SplitN(scanner.Text(), ",", 2)
		positions = append(positions, Coord{MustAtoi(l[1]), MustAtoi(l[0])})
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var grid [71][71]bool
	for i := 0; i < 1024; i++ {
		p := positions[i]
		grid[p.x][p.y] = true
	}

	s := AStar(Coord{0, 0}, Coord{70, 70}, 70, 70, &grid)

	return s
}

func part2() int {
	return 0
}
