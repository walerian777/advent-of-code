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

type Register struct {
	id  byte
	val int
}

type Computer struct {
	a, b, c *Register
	program []int
	ptr     int
	output  []int
}

func (c *Computer) String() string {
	return fmt.Sprintf("[%v: %d]\n[%v: %d]\n[%v: %d]\nPointer: %d\nProgram: %v\n", string(c.a.id), c.a.val, string(c.b.id), c.b.val, string(c.c.id), c.c.val, c.ptr, c.program)
}

func (c *Computer) Execute() {
	for c.ptr < len(c.program) {
		c.NextInstruction()
	}
}

type InstructionFunc func(*Computer)

var Opcodes = map[int]InstructionFunc{
	0: (*Computer).adv,
	1: (*Computer).bxl,
	2: (*Computer).bst,
	3: (*Computer).jnz,
	4: (*Computer).bxc,
	5: (*Computer).out,
	6: (*Computer).bdv,
	7: (*Computer).cdv,
}

func (c *Computer) NextInstruction() {
	f := Opcodes[c.program[c.ptr]]
	f(c)
}

func (c *Computer) Output() string {
	var output []string
	for _, i := range c.output {
		output = append(output, strconv.Itoa(i))
	}
	return strings.Join(output, ",")
}

func (c *Computer) Reset() {
	c.ptr = 0
	c.b.val = 0
	c.c.val = 0
	c.output = []int{}
}

func (c *Computer) adv() {
	c.a.val = c.a.val >> c.comboOperand()
	c.movePointer()
}

func (c *Computer) bxl() {
	c.b.val = c.b.val ^ c.literalOperand()
	c.movePointer()
}

func (c *Computer) bst() {
	c.b.val = c.comboOperand() % 8
	c.movePointer()
}

func (c *Computer) jnz() {
	if c.a.val == 0 {
		c.movePointer()
		return
	}
	c.ptr = c.literalOperand()
}

func (c *Computer) bxc() {
	c.b.val = c.b.val ^ c.c.val
	c.movePointer()
}

func (c *Computer) out() {
	c.output = append(c.output, c.comboOperand()%8)
	c.movePointer()
}

func (c *Computer) bdv() {
	c.b.val = c.a.val >> c.comboOperand()
	c.movePointer()
}

func (c *Computer) cdv() {
	c.c.val = c.a.val >> c.comboOperand()
	c.movePointer()
}

func (c *Computer) literalOperand() int {
	return c.program[c.ptr+1]
}

func (c *Computer) comboOperand() int {
	operand := c.program[c.ptr+1]
	switch operand {
	case 0, 1, 2, 3:
		return operand
	case 4:
		return c.a.val
	case 5:
		return c.b.val
	case 6:
		return c.c.val
	default:
		panic("invalid operand")
	}
}

func (c *Computer) movePointer() {
	c.ptr += 2
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}

func decompile(a int) int {
	return (((((a % 8) ^ 1) ^ 5) ^ (a >> ((a % 8) ^ 1))) % 8)
}

func part1() string {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	c := &Computer{}
	c.a = &Register{'a', MustAtoi(scanner.Text()[12:])}

	scanner.Scan()
	c.b = &Register{'b', MustAtoi(scanner.Text()[12:])}

	scanner.Scan()
	c.c = &Register{'c', MustAtoi(scanner.Text()[12:])}

	scanner.Scan() // Empty line
	scanner.Scan()
	programLine := scanner.Text()[9:]

	if err := scanner.Err(); err != nil {
		panic("scanner errors")
	}
	var program []int
	for _, c := range strings.Split(programLine, ",") {
		program = append(program, MustAtoi(c))
	}
	c.program = program
	c.Execute()

	return c.Output()
}

func part2() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	c := &Computer{}
	c.a = &Register{'a', MustAtoi(scanner.Text()[12:])}

	scanner.Scan()
	c.b = &Register{'b', MustAtoi(scanner.Text()[12:])}

	scanner.Scan()
	c.c = &Register{'c', MustAtoi(scanner.Text()[12:])}

	scanner.Scan() // Empty line
	scanner.Scan()
	programLine := scanner.Text()[9:]

	if err := scanner.Err(); err != nil {
		panic("scanner errors")
	}
	var program []int
	for _, c := range strings.Split(programLine, ",") {
		program = append(program, MustAtoi(c))
	}
	c.program = program

	solutions := map[int]bool{0: true}
	for i := len(program) - 1; i >= 0; i-- {
		n := program[i]
		newSolutions := make(map[int]bool)
		for s := range solutions {
			for j := range 8 {
				a := (s << 3) + j
				if decompile(a) == n {
					newSolutions[a] = true
				}
			}

		}
		solutions = newSolutions

	}

	res := -1
	for k := range solutions {
		if res == -1 || k < res {
			res = k
		}
	}

	return res
}
