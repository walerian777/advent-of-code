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

func (c *Coord) sub(o Coord) Coord {
	return Coord{c.x - o.x, c.y - o.y}
}

func (c *Coord) add(o Coord) Coord {
	return Coord{c.x + o.x, c.y + o.y}
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var x int
	var grid [][]byte
	antennas := make(map[byte][]Coord)
	for scanner.Scan() {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		for y, c := range line {
			if c == '.' {
				continue
			}
			antennas[c] = append(antennas[c], Coord{x, y})
		}
		x++
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1

	antinodes := make(map[Coord]bool)
	for _, as := range antennas {
		for i, a := range as {
			for _, b := range as[i+1:] {
				an1 := a.add(a.sub(b))
				if an1.x >= 0 && an1.x <= maxX && an1.y >= 0 && an1.y <= maxY {
					antinodes[an1] = true
				}
				an2 := b.add(b.sub(a))
				if an2.x >= 0 && an2.x <= maxX && an2.y >= 0 && an2.y <= maxY {
					antinodes[an2] = true
				}
			}
		}
	}

	return len(antinodes)
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var x int
	var grid [][]byte
	antennas := make(map[byte][]Coord)
	for scanner.Scan() {
		line := []byte(scanner.Text())
		grid = append(grid, line)
		for y, c := range line {
			if c == '.' {
				continue
			}
			antennas[c] = append(antennas[c], Coord{x, y})
		}
		x++
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	maxX := len(grid) - 1
	maxY := len(grid[0]) - 1

	antinodes := make(map[Coord]bool)
	for _, as := range antennas {
		for i, a := range as {
			antinodes[a] = true
			for _, b := range as[i+1:] {
				diff1 := a.sub(b)
				an1 := a.add(diff1)
				for an1.x >= 0 && an1.x <= maxX && an1.y >= 0 && an1.y <= maxY {
					antinodes[an1] = true
					an1 = an1.add(diff1)
				}
				diff2 := b.sub(a)
				an2 := b.add(diff2)
				for an2.x >= 0 && an2.x <= maxX && an2.y >= 0 && an2.y <= maxY {
					antinodes[an2] = true
					an2 = an2.add(diff2)
				}
			}
		}
	}

	return len(antinodes)
}
