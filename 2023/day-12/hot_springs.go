package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

const (
	DAMAGED     byte = '#'
	OPERATIONAL byte = '.'
	UNKNOWN     byte = '?'
)

type ConditionRecord struct {
	Springs string
	Groups  []int
}

type State struct {
	CurrentGroup int
	DamagedCount int
}

func (s State) NextGroup() State {
	return State{s.CurrentGroup + 1, 0}
}

func (s State) IncrementDamage() State {
	return State{s.CurrentGroup, s.DamagedCount + 1}
}

func (r *ConditionRecord) String() string {
	return fmt.Sprintf("%v %v", r.Springs, r.Groups)
}

func (r *ConditionRecord) Arrangements() int {
	arrangements := make(map[State]int)
	arrangements[State{}] = 1
	for i := range r.Springs {
		newArrangements := make(map[State]int)
		for s, v := range arrangements {
			if r.Springs[i] != DAMAGED {
				if s.DamagedCount == 0 {
					newArrangements[s] += v

				} else if s.DamagedCount == r.Groups[s.CurrentGroup] {
					newArrangements[s.NextGroup()] += v
				}
			}
			if r.Springs[i] != OPERATIONAL {
				if s.CurrentGroup < len(r.Groups) && s.DamagedCount < r.Groups[s.CurrentGroup] {
					newArrangements[s.IncrementDamage()] += v
				}
			}
		}
		arrangements = newArrangements
	}
	var sum int
	for s, v := range arrangements {
		if r.IsValid(s) {
			sum += v
		}
	}
	return sum
}

func (r *ConditionRecord) IsValid(s State) bool {
	if s.CurrentGroup == len(r.Groups) && s.DamagedCount == 0 {
		return true
	}
	if s.CurrentGroup == len(r.Groups)-1 && s.DamagedCount == r.Groups[s.CurrentGroup] {
		return true
	}
	return false
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var records []*ConditionRecord
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())

		var groups []int
		for _, c := range strings.Split(parts[1], ",") {
			n, err := strconv.Atoi(c)
			if err != nil {
				panic("cannot convert to int")
			}
			groups = append(groups, n)
		}
		records = append(records, &ConditionRecord{parts[0], groups})
	}
	var sum int
	for _, r := range records {
		sum += r.Arrangements()
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

	var records []*ConditionRecord
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		p1 := strings.Repeat(parts[0]+"?", 5)
		p1 = p1[0 : len(p1)-1]

		var groups []int
		for range 5 {
			for _, c := range strings.Split(parts[1], ",") {
				n, err := strconv.Atoi(c)
				if err != nil {
					panic("cannot convert to int")
				}
				groups = append(groups, n)
			}
		}
		records = append(records, &ConditionRecord{p1, groups})
	}
	var sum int
	for _, r := range records {
		sum += r.Arrangements()
	}

	return sum
}
