package main

import (
	"bufio"
	"bytes"
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
	defer func() {
		if err := file.Close(); err != nil {
			panic("cannot close file")
		}
	}()

	scanner := bufio.NewScanner(file)
	var diagram [][]byte

	for scanner.Scan() {
		diagram = append(diagram, []byte(scanner.Text()))
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	atPos := func(p [2]int) byte {
		return diagram[p[0]][p[1]]
	}

	isValid := isValidGenerator(len(diagram), len(diagram[0]))

	start := bytes.IndexByte(diagram[0], 'S')
	var beams [][2]int
	beams = append(beams, toPoint(0, start))
	var cnt int
	for len(beams) > 0 {
		visited := make(map[[2]int]struct{})
		var newBeams [][2]int
		for _, beam := range beams {
			nextBeam := moveDown(beam)
			if !isValid(nextBeam) {
				continue
			}
			if atPos(nextBeam) == '^' {
				cnt++
				leftBeam := moveLeft(nextBeam)
				rightBeam := moveRight(nextBeam)

				if isValid(leftBeam) {
					if _, ok := visited[leftBeam]; !ok {
						visited[leftBeam] = struct{}{}
						newBeams = append(newBeams, leftBeam)
					}
				}
				if isValid(rightBeam) {
					if _, ok := visited[rightBeam]; !ok {
						visited[rightBeam] = struct{}{}
						newBeams = append(newBeams, rightBeam)
					}
				}
			} else {
				if _, ok := visited[nextBeam]; !ok {
					visited[nextBeam] = struct{}{}
					newBeams = append(newBeams, nextBeam)
				}
			}
		}
		beams = newBeams
	}
	return cnt
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer func() {
		if err := file.Close(); err != nil {
			panic("cannot close file")
		}
	}()

	scanner := bufio.NewScanner(file)
	var diagram [][]byte

	for scanner.Scan() {
		diagram = append(diagram, []byte(scanner.Text()))
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	atPos := func(p [2]int) byte {
		return diagram[p[0]][p[1]]
	}

	isValid := isValidGenerator(len(diagram), len(diagram[0]))

	start := bytes.IndexByte(diagram[0], 'S')
	var beams [][2]int
	beams = append(beams, toPoint(0, start))
	counts := make(map[[2]int]int)

	for len(beams) > 0 {
		visited := make(map[[2]int]struct{})
		var newBeams [][2]int
		for _, beam := range beams {
			if _, ok := counts[beam]; !ok {
				counts[beam] = 1
			}
			nextBeam := moveDown(beam)
			if !isValid(nextBeam) {
				continue
			}
			if atPos(nextBeam) == '^' {
				leftBeam := moveLeft(nextBeam)
				rightBeam := moveRight(nextBeam)

				if isValid(leftBeam) {
					if _, ok := counts[leftBeam]; !ok {
						counts[leftBeam] = counts[beam]
					} else {
						counts[leftBeam] += counts[beam]
					}
					if _, ok := visited[leftBeam]; !ok {
						visited[leftBeam] = struct{}{}
						newBeams = append(newBeams, leftBeam)
					}
				}
				if isValid(rightBeam) {
					if _, ok := counts[rightBeam]; !ok {
						counts[rightBeam] = counts[beam]
					} else {
						counts[rightBeam] += counts[beam]
					}
					if _, ok := visited[rightBeam]; !ok {
						visited[rightBeam] = struct{}{}
						newBeams = append(newBeams, rightBeam)
					}
				}
			} else {
				if _, ok := counts[nextBeam]; !ok {
					counts[nextBeam] = counts[beam]
				} else {
					counts[nextBeam] += counts[beam]
				}
				if _, ok := visited[nextBeam]; !ok {
					visited[nextBeam] = struct{}{}
					newBeams = append(newBeams, nextBeam)
				}
			}
		}
		beams = newBeams
	}

	var cnt int
	for p, c := range counts {
		if p[0] == len(diagram) - 1 {
			cnt += c
		}
	}
	return cnt
}

func isValidGenerator(maxX, maxY int) func(p [2]int) bool {
	return func(p [2]int) bool {
		x, y := p[0], p[1]
		if x < 0 || x >= maxX {
			return false
		}
		if y < 0 || y >= maxY {
			return false
		}
		return true
	}
}

func toPoint(x, y int) [2]int {
	return [2]int{x, y}
}

func moveDown(p [2]int) [2]int {
	return [2]int{p[0] + 1, p[1]}
}

func moveLeft(p [2]int) [2]int {
	return [2]int{p[0], p[1] - 1}
}

func moveRight(p [2]int) [2]int {
	return [2]int{p[0], p[1] + 1}
}
