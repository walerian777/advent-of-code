package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

const (
	width  = 101
	height = 103
)

type Robot struct {
	x, y, vx, vy int
}

func (r *Robot) Move() {
	r.x = (r.x + r.vx) % width
	if r.x < 0 {
		r.x += width
	}
	r.y = (r.y + r.vy) % height
	if r.y < 0 {
		r.y += height
	}
}

func (r *Robot) Quandrant() int {
	midW := width / 2
	midH := height / 2
	if r.x < midW && r.y < midH {
		return 0
	}
	if r.x > midW && r.y < midH {
		return 1
	}
	if r.x < midW && r.y > midH {
		return 2
	}
	if r.x > midW && r.y > midH {
		return 3
	}
	return 4
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	re := regexp.MustCompile(`p=(\d+),(\d+) v=(-{0,1}\d+),(-{0,1}\d+)`)
	var robots []*Robot
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindAllStringSubmatch(line, 1)[0]
		r := &Robot{MustAtoi(matches[1]), MustAtoi(matches[2]), MustAtoi(matches[3]), MustAtoi(matches[4])}
		robots = append(robots, r)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	for range 100 {
		for _, r := range robots {
			r.Move()
		}
	}
	var qs [5]int
	for _, r := range robots {
		qs[r.Quandrant()]++
	}

	return qs[0] * qs[1] * qs[2] * qs[3]
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	re := regexp.MustCompile(`p=(\d+),(\d+) v=(-{0,1}\d+),(-{0,1}\d+)`)
	var robots []*Robot
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindAllStringSubmatch(line, 1)[0]
		r := &Robot{MustAtoi(matches[1]), MustAtoi(matches[2]), MustAtoi(matches[3]), MustAtoi(matches[4])}
		robots = append(robots, r)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	minDangerLevel := -1
	minIters := -1

	for iter := 1; iter <= height*width; iter++ {
		var qs [5]int
		for _, r := range robots {
			r.Move()
			qs[r.Quandrant()]++
		}
		dangerLevel := qs[0] * qs[1] * qs[2] * qs[3]
		if minDangerLevel < 0 || dangerLevel < minDangerLevel {
			minDangerLevel = dangerLevel
			minIters = iter
		}
	}

	return minIters
}

func MustAtoi(s string) int {
	n, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}
	return n
}
