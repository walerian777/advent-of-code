package main

import (
	"bufio"
	"fmt"
	"log/slog"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
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
	coord := 0 + 0i
	var corners []complex128

	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		dir := byte(parts[0][0])
		len, err := strconv.Atoi(parts[1])
		if err != nil {
			panic(fmt.Sprintf("cannot convert %s to int", parts[1]))
		}
		coord = nextCoord(coord, dir, float64(len))
		corners = append(corners, coord)
	}

	if err := scanner.Err(); err != nil {
		logger.Error("Scanner error", "error", err)
	}

	return area(corners) + perimeter(corners)/2 + 1
}

func part2() int {
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
	coord := 0 + 0i
	var corners []complex128
	dirs := map[byte]byte{'0': 'R', '1': 'D', '2': 'L', '3': 'U'}

	for scanner.Scan() {
		instruction := strings.Fields(scanner.Text())[2]
		len, err := strconv.ParseInt(instruction[2:7], 16, 0)
		if err != nil {
			panic(fmt.Sprintf("cannot convert %s to int", instruction[:6]))
		}
		dir := dirs[instruction[7]]

		coord = nextCoord(coord, dir, float64(len))
		corners = append(corners, coord)
	}

	if err := scanner.Err(); err != nil {
		logger.Error("Scanner error", "error", err)
	}

	return area(corners) + perimeter(corners)/2 + 1
}

func nextCoord(coord complex128, dir byte, delta float64) complex128 {
	switch dir {
	case 'U':
		return coord - complex(delta, 0)
	case 'R':
		return coord + complex(0, delta)
	case 'D':
		return coord + complex(delta, 0)
	case 'L':
		return coord - complex(0, delta)
	default:
		panic("unknown direction")
	}
}

func area(cs []complex128) int {
	var res float64
	for i := 0; i < len(cs); i++ {
		c1, c2 := cs[i], cs[(i+1)%len(cs)]
		res += (real(c1) * imag(c2)) - (imag(c1) * real(c2))
	}

	if res < 0 {
		res = -res
	}

	return int(res / 2)
}

func perimeter(cs []complex128) int {
	var res float64
	for i := 0; i < len(cs); i++ {
		c1, c2 := cs[i], cs[(i+1)%len(cs)]
		res += math.Sqrt(math.Pow(real(c2)-real(c1), 2) + math.Pow(imag(c2)-imag(c1), 2))
	}

	if res < 0 {
		res = -res
	}

	return int(res)
}
