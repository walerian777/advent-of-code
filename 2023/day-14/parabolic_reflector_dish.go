package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
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

func part2() int {
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
	smallCache := make(map[string]string)
	bigCache := make(map[string][][]byte)
	var scores []int
	indexes := make(map[string]int)
	for iter := range 1_000_000_000 {
		key := join(&grid)
		if r, ok := bigCache[key]; ok {
			grid = r
		} else {
			for range 4 {
				for j := range len(grid[0]) {
					col := takeColumn(&grid, j)
					if v, ok := smallCache[col]; ok {
						for i := range len(grid) {
							grid[i][j] = v[i]
						}
						continue
					}
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

					newCol := takeColumn(&grid, j)
					smallCache[col] = newCol
				}
				grid = rotate(&grid)
			}

			bigCache[key] = grid
		}
		// Cycle detected
		if index, ok := indexes[key]; ok {
			cycleLength := len(indexes)-index
			return scores[(1000000000-index)%cycleLength+index-1]
		}
		indexes[key] = iter
		scores = append(scores, score(&grid))
	}
	return score(&grid)
}

func rotate(grid *[][]byte) [][]byte {
	length := len(*grid)
	newGrid := make([][]byte, length)

	for i := range length {
		newGrid[i] = make([]byte, length)
		for j := range length {
			newGrid[i][j] = (*grid)[length-1-j][i]
		}
	}

	return newGrid
}

func takeColumn(grid *[][]byte, index int) string {
	var bytes []byte
	for i := range len(*grid) {
		bytes = append(bytes, (*grid)[i][index])
	}

	return string(bytes)
}

func join(grid *[][]byte) string {
	length := len(*grid)
	r := make([]string, length)

	for i := range length {
		r = append(r, string((*grid)[i]))
	}
	return strings.Join(r, "")
}

func score(grid *[][]byte) int {
	var sum int
	length := len(*grid)
	for i := range length {
		for j := range length {
			if (*grid)[i][j] == ROUNDED {
				sum += length - i
			}
		}
	}
	return sum
}
