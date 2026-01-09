package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(part1())
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

	var ps []Pos

	for scanner.Scan() {
		l := scanner.Text()
		nums := strings.SplitN(l, ",", 3)
		ps = append(ps, Pos{toIntOrPanic(nums[0]), toIntOrPanic(nums[1]), toIntOrPanic(nums[2])})
	}
	if err := scanner.Err(); err != nil {
		panic("scanner err")
	}

	var conns []Conn
	visited := make(map[Conn]struct{})

	for _, p := range ps {
		for _, q := range ps {
			if p == q {
				continue
			}
			if _, ok := visited[Conn{p.X, p.Y, p.Z, q.X, q.Y, q.Z}]; ok {
				continue
			}
			if _, ok := visited[Conn{q.X, q.Y, q.Z, p.X, p.Y, p.Z}]; ok {
				continue
			}
			visited[Conn{p.X, p.Y, p.Z, q.X, q.Y, q.Z}] = struct{}{}
			conns = append(conns, Conn{p.X, p.Y, p.Z, q.X, q.Y, q.Z})
		}
	}
	slices.SortFunc(conns, func(a, b Conn) int {
		d_a := distance(a.Px, a.Py, a.Pz, a.Qx, a.Qy, a.Qz)
		d_b := distance(b.Px, b.Py, b.Pz, b.Qx, b.Qy, b.Qz)

		return int((d_a - d_b) * 100000)
	})

	fmt.Println(conns[0:3])

	var id int

	circuits := make(map[int][]Pos)
	connected := make(map[Pos]int)

	for i := 0; i < 1000; i++ {
		top := conns[0]
		conns = conns[1:]
		p := top.P()
		q := top.Q()
		id1, ok1 := connected[p]
		id2, ok2 := connected[q]

		if !ok1 && !ok2 { // New circuit
			id++
			nextId := id
			circuits[nextId] = []Pos{p, q}
			connected[p] = nextId
			connected[q] = nextId
		} else if ok1 && ok2 { // Both already connected
			if id1 == id2 { // And in the same circuit
				continue
			}
			// Merge two circuits
			for _, o := range circuits[id2] {
				connected[o] = id1
				circuits[id1] = append(circuits[id1], o)
			}
			delete(circuits, id2)
		} else if ok1 && !ok2 {
			connected[q] = id1
			circuits[id1] = append(circuits[id1], q)
		} else if !ok1 && ok2 {
			connected[p] = id2
			circuits[id2] = append(circuits[id2], p)
		} else {
			panic("unavailable")
		}
	}

	var lens []int

	for _, c := range circuits {
		lens = append(lens, len(c))
	}

	slices.Sort(lens)
	l := len(lens)

	return lens[l-1] * lens[l-2] * lens[l-3]
}

type Pos struct {
	X, Y, Z int
}

type Conn struct {
	Px, Py, Pz int
	Qx, Qy, Qz int
}

func (c Conn) P() Pos {
	return Pos{c.Px, c.Py, c.Pz}
}

func (c Conn) Q() Pos {
	return Pos{c.Qx, c.Qy, c.Qz}
}

type Circuit struct {
	Boxes map[Pos]struct{}
}

func dist(p, q Pos) float64 {
	dx := float64(p.X - q.X)
	dy := float64(p.Y - q.Y)
	dz := float64(p.Z - q.Z)

	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func distance(px, py, pz, qx, qy, qz int) float64 {
	dx := float64(px - qx)
	dy := float64(py - qy)
	dz := float64(pz - qz)

	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func toIntOrPanic(s string) int {
	num, err := strconv.Atoi(s)
	if err != nil {
		panic("cannot conv to int")
	}

	return num
}
