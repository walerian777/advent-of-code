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

func part2() string {
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

	maxClique := BronKerbosch(comps)
	slices.Sort(maxClique)

	return strings.Join(maxClique, ",")
}

func BronKerbosch(graph map[string]*Comp) []string {
	p := make(map[string]bool)
	for k := range graph {
		p[k] = true
	}

	var cliques [][]string
	BronKerboschNested(make(map[string]bool), p, make(map[string]bool), &cliques, func(id string) []string {
		var ids []string
		for c := range graph[id].conn {
			ids = append(ids, c.id)
		}
		return ids
	})

	slices.SortFunc(cliques, func(a, b []string) int {
		return len(b) - len(a)
	})

	return cliques[0]

}

func BronKerboschNested(r, p, x map[string]bool, cliques *[][]string, connFunc func(string) []string) {
	if len(p)+len(x) == 0 {
		var clique []string
		for k := range r {
			clique = append(clique, k)
		}
		*cliques = append(*cliques, clique)
	}
	for v := range p {
		newR := make(map[string]bool)
		newR[v] = true
		for k := range r {
			newR[k] = true
		}
		newP := make(map[string]bool)
		newX := make(map[string]bool)
		for _, c := range connFunc(v) {
			if _, ok := p[c]; ok {
				newP[c] = true
			}
			if _, ok := x[c]; ok {
				newX[c] = true
			}
		}
		BronKerboschNested(newR, newP, newX, cliques, connFunc)
		delete(p, v)
		x[v] = true
	}
}
