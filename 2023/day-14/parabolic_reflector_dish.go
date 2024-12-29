package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fmt.Println(part1())
}

const (
	ROUNDED byte = 'O'
	CUBE    byte = '#'
	EMPTY   byte = '.'
)

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var grid [][]byte
	for scanner.Scan() {
		grid = append(grid, []byte(scanner.Text()))
	}

	lastEmpty := -1
	var roundedCount int
	for j := range len(grid[0]) {
		for i := range len(grid) {
			switch grid[i][j] {
			case ROUNDED:
				if lastEmpty != -1 {
					roundedCount++
				}
			case CUBE:
				if lastEmpty != -1 && roundedCount > 0 {
					for roundedCount > 0 && lastEmpty < len(grid) {
						grid[lastEmpty][j] = ROUNDED
						lastEmpty++
						roundedCount--
					}
					for lastEmpty < i {
						grid[lastEmpty][j] = EMPTY
						lastEmpty++
					}
				}
				lastEmpty = -1
			case EMPTY:
				if lastEmpty == -1 {
					lastEmpty = i
				}
			default:
				panic("unknown tile type")
			}
			if i == len(grid)-1 {
				if lastEmpty != -1 && roundedCount > 0 {
					for roundedCount > 0 && lastEmpty < len(grid) {
						grid[lastEmpty][j] = ROUNDED
						lastEmpty++
						roundedCount--
					}
					for lastEmpty <= i {
						grid[lastEmpty][j] = EMPTY
						lastEmpty++
					}
				}
			}
		}
		lastEmpty = -1
	}

	var sum int
	length := len(grid)
	for i := range grid {
		for j := range grid[i] {
			if grid[i][j] == ROUNDED {
				sum += length - i
			}
		}
	}
	return sum
}
