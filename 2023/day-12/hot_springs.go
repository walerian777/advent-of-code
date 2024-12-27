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
}

const (
	DAMAGED     string = "#"
	OPERATIONAL string = "."
	UNKNOWN     string = "?"
)

type ConditionRecord struct {
	Springs string
	Groups  []int
}

func (r *ConditionRecord) String() string {
	return fmt.Sprintf("%v %v", r.Springs, r.Groups)
}

func (r *ConditionRecord) Arrangements() int {
	return r.FindArrangements(r.Springs)
}

func (r *ConditionRecord) FindArrangements(curr string) int {
	if strings.Index(curr, UNKNOWN) < 0 {
		if r.IsValid(curr) {
			return 1
		}
		return 0
	}
	dmgd := strings.Replace(curr, UNKNOWN, DAMAGED, 1)
	optl := strings.Replace(curr, UNKNOWN, OPERATIONAL, 1)
	return r.FindArrangements(dmgd) + r.FindArrangements(optl)
}

func (r *ConditionRecord) IsValid(arrangement string) bool {
	var i int
	for _, s := range strings.Split(arrangement, OPERATIONAL) {
		if len(s) == 0 {
			continue
		}
		if i >= len(r.Groups) || len(s) != r.Groups[i] {
			return false
		}
		i++

	}

	return i == len(r.Groups)
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
