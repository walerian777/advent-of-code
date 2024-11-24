package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(recordBeatingNumbers())
	fmt.Println(recordBeatingLongerRace())
}

type Millisecond int
type Millimeter int

type Race struct {
	Time Millisecond
	Dist Millimeter
}

func (r *Race) isBeating(waitTime Millisecond) bool {
	t := r.Time - waitTime
	return Millimeter(t*waitTime) > r.Dist
}

func recordBeatingNumbers() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	timesLine := strings.Fields(scanner.Text())

	scanner.Scan()
	distsLine := strings.Fields(scanner.Text())

	var races []*Race
	for i := 1; i < len(timesLine); i++ {
		time, err := strconv.Atoi(timesLine[i])
		if err != nil {
			panic("cannot parse number")
		}
		dist, err := strconv.Atoi(distsLine[i])
		if err != nil {
			panic("cannot parse number")
		}
		races = append(races, &Race{Time: Millisecond(time), Dist: Millimeter(dist)})
	}

	mult := 1
	for _, race := range races {
		var s Millisecond
		for i := Millisecond(1); i < race.Time; i++ {
			if race.isBeating(i) {
				s += i
				break
			}
		}
		for i := race.Time - 1; i > 0; i-- {
			if race.isBeating(i) {
				s = i - s
				break
			}
		}
		mult *= int(s + 1)
	}

	return mult
}

func recordBeatingLongerRace() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	timeLine := strings.Join(strings.Fields(scanner.Text())[1:], "")

	scanner.Scan()
	distLine := strings.Join(strings.Fields(scanner.Text())[1:], "")

	time, err := strconv.Atoi(timeLine)
	if err != nil {
		panic("cannot parse number")
	}
	dist, err := strconv.Atoi(distLine)
	if err != nil {
		panic("cannot parse number")
	}

	race := &Race{Time: Millisecond(time), Dist: Millimeter(dist)}

	var s Millisecond
	for i := Millisecond(1); i < race.Time; i++ {
		if race.isBeating(i) {
			s = i
			break
		}
	}
	for i := race.Time - 1; i > 0; i-- {
		if race.isBeating(i) {
			s = i - s
			break
		}
	}

	return int(s) + 1
}
