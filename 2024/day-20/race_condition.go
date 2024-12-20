package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"
)

func main() {
	fmt.Println(part1())
}

type Coord struct {
	x, y int
}

func AStar(start, goal Coord, x, y int, grid *[][]byte) ([]Coord, int, error) {
	openSet := map[Coord]bool{start: true}
	gScore := map[Coord]int{start: 0}
	fScore := map[Coord]int{start: manhattanDistance(start, goal)}
	distFunc := manhattanDistance
	cameFrom := make(map[Coord]Coord)

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
			return reconstructPath(cameFrom, current), gScore[current], nil
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
				cameFrom[nb] = Coord{current.x, current.y}
				gScore[nb] = gScoreTentative
				fScore[nb] = gScoreTentative + manhattanDistance(nb, goal)
				if _, ok := openSet[nb]; !ok {
					openSet[nb] = true
				}
			}
		}
	}
	return nil, 0, fmt.Errorf("Couldn't find the path")
}

func AStarCheat(start, goal, cheat Coord, x, y int, grid *[][]byte) ([]Coord, int, error) {
	openSet := map[Coord]bool{start: true}
	gScore := map[Coord]int{start: 0}
	fScore := map[Coord]int{start: manhattanDistance(start, goal)}
	distFunc := manhattanDistance
	cameFrom := make(map[Coord]Coord)

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
			return reconstructPath(cameFrom, current), gScore[current], nil
		}
		delete(openSet, current)
		for _, nb := range neighbors(current) {
			if nb.x < 0 || nb.y < 0 || nb.x > x || nb.y > y {
				continue
			}
			if (*grid)[nb.x][nb.y] == '#' {
				if nb != cheat {
					continue
				}
			}

			gScoreTentative := gScore[current] + distFunc(current, nb)
			if gScoreCurrent, ok := gScore[nb]; !ok || gScoreTentative < gScoreCurrent {
				cameFrom[nb] = Coord{current.x, current.y}
				gScore[nb] = gScoreTentative
				fScore[nb] = gScoreTentative + manhattanDistance(nb, goal)
				if _, ok := openSet[nb]; !ok {
					openSet[nb] = true
				}
			}
		}
	}
	return nil, 0, fmt.Errorf("Couldn't find the path")
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
	var grid [][]byte
	var start, end *Coord
	for i := 0; scanner.Scan(); i++ {
		line := scanner.Text()
		grid = append(grid, []byte(line))
		if start == nil {
			if j := strings.Index(line, "S"); j > 0 {
				start = &Coord{i, j}
			}
		}
		if end == nil {
			if j := strings.Index(line, "E"); j > 0 {
				end = &Coord{i, j}
			}
		}
	}
	maxX, maxY := len(grid)-1, len(grid[0])-1

	path, score, _ := AStar(*start, *end, maxX, maxY, &grid)
	cheats := make(map[Coord]int)
	for _, p := range path {
		for _, nb := range neighbors(p) {
			if grid[nb.x][nb.y] != '#' {
				continue
			}
			if _, ok := cheats[nb]; ok {
				continue
			}
			_, newScore, err := AStarCheat(*start, *end, nb, maxX, maxY, &grid)
			if err != nil {
				fmt.Println("Cannot do A* for ", nb)
			}
			cheats[nb] = score - newScore
		}
	}
	var sum int
	for _, v := range cheats {
		if v >= 100 {
			sum++
		}
	}
	return sum
}
