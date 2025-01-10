package main

import (
	"bufio"
	"fmt"
	"log/slog"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
}

func part1() int {
	logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer func() {
		err := file.Close()
		if err != nil {
			logger.Error("Failed to close the file", "error", err)
		}
	}()

	scanner := bufio.NewScanner(file)
	grid := make(map[complex128]struct{})
	coord := 0 + 0i
	var corners []complex128
	var maxX, maxY float64

	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		dir := byte(parts[0][0])
		len, err := strconv.Atoi(parts[1])
		if err != nil {
			panic(fmt.Sprintf("cannot convert %s to int", parts[1]))
		}
		for range len {
			coord = nextCoord(coord, dir)
			grid[coord] = struct{}{}
		}
		if real(coord) > maxX {
			maxX = real(coord)
		}
		if imag(coord) > maxY {
			maxY = imag(coord)
		}
		corners = append(corners, coord)
	}

	if err := scanner.Err(); err != nil {
		logger.Error("Scanner error", "error", err)
	}

	return int(area(corners)) + len(grid)/2 + 1
}

func nextCoord(coord complex128, dir byte) complex128 {
	switch dir {
	case 'U':
		return coord - 1 + 0i
	case 'R':
		return coord + 0 + 1i
	case 'D':
		return coord + 1 + 0i
	case 'L':
		return coord + 0 - 1i
	default:
		panic("unknown direction")
	}
}

func area(cs []complex128) float64 {
	var res float64
	for i := 0; i < len(cs); i++ {
		c1, c2 := cs[i], cs[(i+1)%len(cs)]
		res += (real(c1) * imag(c2)) - (imag(c1) * real(c2))
	}

	if res < 0 {
		res = -res
	}

	return res / 2
}
