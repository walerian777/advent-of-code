package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strings"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

type Comp struct {
	id   string
	conn map[*Comp]bool
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open input")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	comps := make(map[string]*Comp)

	for scanner.Scan() {
		conn := strings.SplitN(scanner.Text(), "-", 2)
		c1, c2 := conn[0], conn[1]

		if _, ok := comps[c1]; !ok {
			comps[c1] = &Comp{id: c1, conn: map[*Comp]bool{}}
		}
		if _, ok := comps[c2]; !ok {
			comps[c2] = &Comp{id: c2, conn: map[*Comp]bool{}}
		}
		comps[c1].conn[comps[c2]] = true
		comps[c2].conn[comps[c1]] = true
	}

	lans := make(map[string]bool)
	for _, c := range comps {
		if c.id[0] != 't' {
			continue
		}
		for connC := range c.conn {
			for connConnC := range connC.conn {
				if _, ok := c.conn[connConnC]; ok {
					ids := []string{c.id, connC.id, connConnC.id}
					slices.Sort(ids)
					k := strings.Join(ids, "-")
					lans[k] = true
				}
			}
		}
	}

	return len(lans)
}

func part2() int {
	return 0
}
