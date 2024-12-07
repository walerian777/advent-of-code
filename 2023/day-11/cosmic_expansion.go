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
	x, y int
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var galaxies []Coord
	regularXs := make(map[int]bool)
	regularYs := make(map[int]bool)
	var x, y int
	for scanner.Scan() {
		line := []byte(scanner.Text())
		y = 0
		for _, c := range line {
			if c == '#' {
				galaxies = append(galaxies, Coord{x, y})
				regularYs[y] = true
				regularXs[x] = true
			}
			y++
		}
		x++
	}

	var adjustedGalaxies []Coord
	for _, g := range galaxies {
		newX := g.x
		newY := g.y
		for i := 0; i < g.x; i++ {
			if _, ok := regularXs[i]; !ok {
				newX++
			}
		}
		for j := 0; j < g.y; j++ {
			if _, ok := regularYs[j]; !ok {
				newY++
			}
		}
		adjustedGalaxies = append(adjustedGalaxies, Coord{newX, newY})
	}

	var sum int
	for i := 0; i < len(adjustedGalaxies); i++ {
		for j := i + 1; j < len(adjustedGalaxies); j++ {
			sc := manhattanDistance(adjustedGalaxies[i], adjustedGalaxies[j])
			sum += sc
		}
	}

	return sum
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var galaxies []Coord
	regularXs := make(map[int]bool)
	regularYs := make(map[int]bool)
	var x, y int
	for scanner.Scan() {
		line := []byte(scanner.Text())
		y = 0
		for _, c := range line {
			if c == '#' {
				galaxies = append(galaxies, Coord{x, y})
				regularYs[y] = true
				regularXs[x] = true
			}
			y++
		}
		x++
	}

	var adjustedGalaxies []Coord
	for _, g := range galaxies {
		newX := g.x
		newY := g.y
		for i := 0; i < g.x; i++ {
			if _, ok := regularXs[i]; !ok {
				newX += 1000000 - 1
			}
		}
		for j := 0; j < g.y; j++ {
			if _, ok := regularYs[j]; !ok {
				newY += 1000000 - 1
			}
		}
		adjustedGalaxies = append(adjustedGalaxies, Coord{newX, newY})
	}

	var sum int
	for i := 0; i < len(adjustedGalaxies); i++ {
		for j := i + 1; j < len(adjustedGalaxies); j++ {
			sc := manhattanDistance(adjustedGalaxies[i], adjustedGalaxies[j])
			sum += sc
		}
	}

	return sum
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
