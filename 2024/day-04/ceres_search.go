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

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	var grid [][]byte
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []byte(line))
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	maxW := len(grid) - 1
	maxH := len(grid[0]) - 1
	var sum int
	for x, row := range grid {
		for y, c := range row {
			if c == 'X' {
				var subsum int

				// east
				if y+3 <= maxW {
					if row[y+1] == 'M' && row[y+2] == 'A' && row[y+3] == 'S' {
						subsum++
					}
				}
				// west
				if y-3 >= 0 {
					if row[y-1] == 'M' && row[y-2] == 'A' && row[y-3] == 'S' {
						subsum++
					}
				}
				// north
				if x-3 >= 0 {
					if grid[x-1][y] == 'M' && grid[x-2][y] == 'A' && grid[x-3][y] == 'S' {
						subsum++
					}
				}
				// south
				if x+3 <= maxH {
					if grid[x+1][y] == 'M' && grid[x+2][y] == 'A' && grid[x+3][y] == 'S' {
						subsum++
					}
				}
				// south-east
				if x+3 <= maxH && y+3 <= maxW {
					if grid[x+1][y+1] == 'M' && grid[x+2][y+2] == 'A' && grid[x+3][y+3] == 'S' {
						subsum++
					}
				}
				// south-west
				if x+3 <= maxH && y-3 >= 0 {
					if grid[x+1][y-1] == 'M' && grid[x+2][y-2] == 'A' && grid[x+3][y-3] == 'S' {
						subsum++
					}
				}
				// north-east
				if x-3 >= 0 && y+3 <= maxW {
					if grid[x-1][y+1] == 'M' && grid[x-2][y+2] == 'A' && grid[x-3][y+3] == 'S' {
						subsum++
					}
				}
				// north-west
				if x-3 >= 0 && y-3 >= 0 {
					if grid[x-1][y-1] == 'M' && grid[x-2][y-2] == 'A' && grid[x-3][y-3] == 'S' {
						subsum++
					}
				}
				sum += subsum
			}
		}
	}

	return sum
}

func part2() int {
	return 0
}
