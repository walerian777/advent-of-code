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
	// fmt.Println(part2())
}

type Rule struct {
	Cond func(map[byte]int) bool
	Out  string
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open the input file")
	}
	defer func() {
		err := file.Close()
		if err != nil {
			fmt.Println(fmt.Errorf("cannot close input file %v", err))
		}
	}()
	scanner := bufio.NewScanner(file)
	readingWorkflows := true
	workflows := make(map[string][]Rule)
	var sum int
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			readingWorkflows = false
			continue
		}
		if readingWorkflows {
			split := strings.SplitN(line, "{", 2)
			name, rest := split[0], split[1]
			workflows[name] = []Rule{}
			for _, r := range strings.Split(rest[:len(rest)-1], ",") {
				if strings.IndexByte(r, ':') < 0 {
					workflows[name] = append(workflows[name], Rule{Out: r, Cond: func(_ map[byte]int) bool { return true }})
					continue
				}
				if r[1] == '<' {
					rs := strings.SplitN(r[2:], ":", 2)
					val, err := strconv.Atoi(rs[0])
					if err != nil {
						panic("cannot convert string to int")
					}
					workflows[name] = append(workflows[name], Rule{Out: rs[1], Cond: func(mm map[byte]int) bool { return mm[r[0]] < val }})
					continue
				}
				if r[1] == '>' {
					rs := strings.SplitN(r[2:], ":", 2)
					val, err := strconv.Atoi(rs[0])
					if err != nil {
						panic("cannot convert string to int")
					}
					workflows[name] = append(workflows[name], Rule{Out: rs[1], Cond: func(mm map[byte]int) bool { return mm[r[0]] > val }})
					continue
				}
			}
		} else {
			split := strings.SplitN(line[1:len(line)-1], ",", 4)
			ratings := make(map[byte]int, 4)
			for _, r := range split {
				val, err := strconv.Atoi(r[2:])
				if err != nil {
					panic("cannot convert string to int")
				}
				ratings[r[0]] = val
			}
			out := "in"
			for {
				for _, rule := range workflows[out] {
					if rule.Cond(ratings) {
						out = rule.Out
						break
					}
				}
				if out == "R" {
					break
				}
				if out == "A" {
					sum += ratings['x'] + ratings['m'] + ratings['a'] + ratings['s']
					break
				}
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}
	return sum
}
