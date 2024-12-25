package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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
	defer file.Close()

	scanner := bufio.NewScanner(file)

	wires := make(map[string]int)
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			break
		}
		wires[l[:3]] = MustAtoi(l[5:6])
	}

	var ops []string
	for scanner.Scan() {
		ops = append(ops, scanner.Text())
	}
	var done int
	for done < len(ops) {
		for _, l := range ops {
			switch l[4] {
			case 'A':
				in1, in2, out := l[0:3], l[8:11], l[15:18]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] & wires[in2]
				done++
			case 'O':
				in1, in2, out := l[0:3], l[7:10], l[14:17]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] | wires[in2]
				done++
			case 'X':
				in1, in2, out := l[0:3], l[8:11], l[15:18]
				if _, ok := wires[out]; ok {
					continue
				}
				if _, ok := wires[in1]; !ok {
					continue
				}
				if _, ok := wires[in2]; !ok {
					continue
				}
				wires[out] = wires[in1] ^ wires[in2]
				done++
			}
		}
	}
	var z int
	for i := range 46 {
		var k string
		if i < 10 {
			k = fmt.Sprintf("z0%d", i)
		} else {
			k = fmt.Sprintf("z%d", i)
		}
		z += wires[k] << i
	}

	return z
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	wires := make(map[string]int)
	for scanner.Scan() {
		l := scanner.Text()
		if l == "" {
			break
		}
		wires[l[:3]] = MustAtoi(l[5:6])

	}

	outToOp := make(map[string]Op)
	for scanner.Scan() {
		l := scanner.Text()
		switch l[4] {
		case 'A':
			in1, in2, out := l[0:3], l[8:11], l[15:18]
			outToOp[out] = Op{in1, in2, "&"}

		case 'O':
			in1, in2, out := l[0:3], l[7:10], l[14:17]

			outToOp[out] = Op{in1, in2, "|"}
		case 'X':
			in1, in2, out := l[0:3], l[8:11], l[15:18]
			outToOp[out] = Op{in1, in2, "^"}

		}
	}

	var x int
	var y int
	for i := range 45 {
		var kx, ky string
		if i < 10 {
			kx = fmt.Sprintf("x0%d", i)
			ky = fmt.Sprintf("y0%d", i)
		} else {
			kx = fmt.Sprintf("x%d", i)
			ky = fmt.Sprintf("y%d", i)
		}
		x += wires[kx] << i
		y += wires[ky] << i
	}
	z := part1()

	fmt.Println(strconv.FormatInt(int64(x), 2))
	fmt.Println("+")
	fmt.Println(strconv.FormatInt(int64(y), 2))
	fmt.Println("=")
	fmt.Println(strconv.FormatInt(int64(x+y), 2))
	fmt.Println("vs")
	fmt.Println(strconv.FormatInt(int64(z), 2))

	for i := range 46 {
		var k string
		if i < 10 {
			k = fmt.Sprintf("z0%d", i)
		} else {
			k = fmt.Sprintf("z%d", i)
		}

		fmt.Println(Eval(k, 0, 3, outToOp))
	}

	return x + y - z
}

type Op struct {
	in1, in2, op string
}

func Eval(wire string, depth, maxDepth int, ops map[string]Op) string {
	if depth >= maxDepth || wire[0] == 'x' || wire[0] == 'y' {
		return wire
	}
	op := ops[wire]
	in1 := Eval(op.in1, depth+1, maxDepth, ops)

	in2 := Eval(op.in2, depth+1, maxDepth, ops)
	return fmt.Sprintf("%v: (%v) %v (%v)", wire, in1, op.op, in2)
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}
