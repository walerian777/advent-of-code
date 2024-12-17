package main

import (
	"bufio"
	"fmt"
	"math"
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

func (c Coord) String() string {
	return fmt.Sprintf("(%d, %d) [%v]", c.x, c.y, string(c.dir))
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var grid [][]byte
	var nodes []Coord
	var reindeer Coord
	var reindeerFound bool
	var end Coord
	var endFound bool
	for i := 0; scanner.Scan(); i++ {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		if !reindeerFound {
			if j := slices.Index(line, 'S'); j > -1 {
				reindeer = Coord{i, j, 'e'}
				reindeerFound = true
			}
		}
		if !endFound {
			if j := slices.Index(line, 'E'); j > -1 {
				end = Coord{i, j, 'e'}
				endFound = true
			}
		}
		for j, _ := range line {
			nodes = append(nodes, Coord{i, j, 'x'})
		}
	}
	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1

	_, sc := aStar(reindeer, end, maxX, maxY, &grid, func(a, b Coord) int {
		d := 1
		if a.dir != b.dir {
			d += 1000
		}
		return d
	})

	return sc
}

func part2() int {
	return 0
}

type DistFunc func(Coord, Coord) int

func aStar(start, goal Coord, x, y int, grid *[][]byte, distFunc DistFunc) ([]Coord, int) {
	openSet := map[Coord]bool{start: true}
	cameFrom := make(map[Coord]Coord)
	gScore := map[Coord]int{start: 0}
	fScore := map[Coord]int{start: manhattanDistance(start, goal)}

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
			return reconstructPath(cameFrom, current), gScore[current]
		}
		delete(openSet, current)
		for _, nb := range neighbors(current) {
			if nb.x < 0 || nb.y < 0 || nb.x > x || nb.y > y {
				continue
			}
			if (*grid)[nb.x][nb.y] == '#' {
				continue
			}

			gScoreTentative := gScore[current] + distFunc(current, nb)
			if gScoreCurrent, ok := gScore[nb]; !ok || gScoreTentative < gScoreCurrent {

				cameFrom[nb] = Coord{current.x, current.y, nb.dir}
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

func reconstructPath(cameFrom map[Coord]Coord, current Coord) []Coord {
	totalPath := []Coord{current}
	k := current
	for {
		nK, ok := cameFrom[k]
		if !ok {
			break
		}
		totalPath = append(totalPath, nK)
		k = nK
	}
	return totalPath
}

func neighbors(c Coord) []Coord {
	return []Coord{
		c.plus(Coord{-1, 0, 'n'}),
		c.plus(Coord{0, -1, 'w'}),
		c.plus(Coord{0, 1, 'e'}),
		c.plus(Coord{1, 0, 's'}),
	}
}

func (c Coord) plus(o Coord) Coord {
	return Coord{c.x + o.x, c.y + o.y, o.dir}
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
