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

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	grid := make(map[complex128]int)
	var rows, cols int
	for i := 0; scanner.Scan(); i++ {
		rows = i
		l := scanner.Text()
		for j := range l {
			cols = j
			v, err := strconv.Atoi(string(l[j]))
			if err != nil {
				panic("strconv")
			}

			grid[complex(float64(i), float64(j))] = v
		}
	}

	source := 0 + 0i
	target := complex(float64(rows), float64(cols))
	return FindPath(source, target, 0, 3, grid)
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	grid := make(map[complex128]int)
	var rows, cols int
	for i := 0; scanner.Scan(); i++ {
		rows = i
		l := scanner.Text()
		for j := range l {
			cols = j
			v, err := strconv.Atoi(string(l[j]))
			if err != nil {
				panic("strconv")
			}

			grid[complex(float64(i), float64(j))] = v
		}
	}

	source := 0 + 0i
	target := complex(float64(rows), float64(cols))
	return FindPath(source, target, 4, 10, grid)
}

type Vertex struct {
	v complex128
	d byte
}

func (ver Vertex) Move(i int) Vertex {
	delta := float64(i)
	v := ver.v
	switch ver.d {
	case 'v':
		v += complex(delta, 0)
	case '^':
		v += complex(-delta, 0)
	case '>':
		v += complex(0, delta)
	case '<':
		v += complex(0, -delta)
	default:
		panic("unknown direction!")
	}
	return Vertex{v, ver.d}
}

func (ver Vertex) Neighbors() []Vertex {
	switch ver.d {
	case byte(0):
		return []Vertex{{ver.v + (0 + 1i), '>'}, {ver.v + (1 + 0i), 'v'}}
	case 'v':
		return []Vertex{{ver.v + (0 + 1i), '>'}, {ver.v + (0 - 1i), '<'}}
	case '^':
		return []Vertex{{ver.v + (0 + 1i), '>'}, {ver.v + (0 - 1i), '<'}}
	case '>':
		return []Vertex{{ver.v + (1 + 0i), 'v'}, {ver.v + (-1 + 0i), '^'}}
	case '<':
		return []Vertex{{ver.v + (1 + 0i), 'v'}, {ver.v + (-1 + 0i), '^'}}
	default:
		panic("unknown direction!")
	}
}

func FindPath(source, target complex128, minSteps, maxSteps int, grid map[complex128]int) int {
	h := func(vertex Vertex) int {
		v := vertex.v
		return int(real(target)-real(v)+imag(target)-imag(v)) + grid[v]
	}
	sourceV := Vertex{v: source}
	queue := map[Vertex]struct{}{sourceV: struct{}{}}
	dists := make(map[Vertex]int)
	heurs := make(map[Vertex]int)

	dists[sourceV] = 0
	heurs[sourceV] = h(sourceV)

	for len(queue) > 0 {
		curr := Vertex{}
		for k := range queue {
			if curr == (Vertex{}) || heurs[k] < heurs[curr] {
				curr = k
			}
		}
		if curr.v == target {
			return dists[curr]
		}

		delete(queue, curr)

		for _, next := range curr.Neighbors() {
			maybeDist := dists[curr]
			for i := range maxSteps {
				n := next.Move(i)
				if _, ok := grid[n.v]; !ok {
					continue
				}
				maybeDist += grid[n.v]
				if i < minSteps-1 {
					continue
				}
				if neighborDist, ok := dists[n]; !ok || maybeDist < neighborDist {
					dists[n] = maybeDist
					heurs[n] = maybeDist + h(n)
					queue[n] = struct{}{}
				}
			}
		}

	}

	panic("Failed to find the path")
}
