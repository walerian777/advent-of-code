package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	fmt.Println(part1())
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var pins []string
	for scanner.Scan() {
		pins = append(pins, scanner.Text())
	}

	var sum int
	for _, pin := range pins {
		sum += complexity(pin)
	}
	return sum
}

func complexity(pin string) int {
	seq := getSequencesForNum(pin)
	sequence := minLengthSequence(seq, 2)
	numericPart := pin[0:3]
	numeric, err := strconv.Atoi(numericPart)
	if err != nil {
		panic("cannot convert to int")
	}
	return sequence * numeric
}

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
// |   | 0 | A |
// +---+---+---+
var numKeypad = map[byte][2]int{
	'7': {0, 3},
	'8': {1, 3},
	'9': {2, 3},
	'4': {0, 2},
	'5': {1, 2},
	'6': {2, 2},
	'1': {0, 1},
	'2': {1, 1},
	'3': {2, 1},
	'0': {1, 0},
	'A': {2, 0},
}

// +---+---+---+
// |   | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
var dirKeypad = map[byte][2]int{
	'^': {1, 1},
	'A': {2, 1},
	'<': {0, 0},
	'v': {1, 0},
	'>': {2, 0},
}

func minLengthSequence(sequence []byte, iter int) int {
	seq := getSequencesForDir(sequence)

	if iter == 1 {
		return len(seq)
	}

	totalLength := 0
	for _, s := range splitSeq(seq) {
		length := minLengthSequence(s, iter-1)
		totalLength += length
	}
	return totalLength
}

func getSequencesForNum(pin string) []byte {
	curr := numKeypad['A']
	var seq []byte

	for i := range pin {
		next := numKeypad[pin[i]]
		dX, dY := next[0]-curr[0], next[1]-curr[1]
		var horizontal, vertical []byte

		for i := 0; i < abs(dX); i++ {
			if dX >= 0 {
				horizontal = append(horizontal, '>')
			} else {
				horizontal = append(horizontal, '<')
			}
		}

		for i := 0; i < abs(dY); i++ {
			if dY >= 0 {
				vertical = append(vertical, '^')
			} else {
				vertical = append(vertical, 'v')
			}
		}

		if curr[1] == 0 && next[0] == 0 {
			seq = append(seq, vertical...)
			seq = append(seq, horizontal...)
		} else if curr[0] == 0 && next[1] == 0 {
			seq = append(seq, horizontal...)
			seq = append(seq, vertical...)
		} else if dX < 0 {
			seq = append(seq, horizontal...)
			seq = append(seq, vertical...)
		} else if dX >= 0 {
			seq = append(seq, vertical...)
			seq = append(seq, horizontal...)
		}

		curr = next
		seq = append(seq, 'A')
	}
	return seq
}

func getSequencesForDir(sequence []byte) []byte {
	curr := dirKeypad['A']
	var seq []byte

	for i := range sequence {
		next := dirKeypad[sequence[i]]
		dX, dY := next[0]-curr[0], next[1]-curr[1]
		var horizontal, vertical []byte

		for i := 0; i < abs(dX); i++ {
			if dX >= 0 {
				horizontal = append(horizontal, '>')
			} else {
				horizontal = append(horizontal, '<')
			}
		}

		for i := 0; i < abs(dY); i++ {
			if dY >= 0 {
				vertical = append(vertical, '^')
			} else {
				vertical = append(vertical, 'v')
			}
		}

		if curr[0] == 0 && next[1] == 1 {
			seq = append(seq, horizontal...)
			seq = append(seq, vertical...)
		} else if curr[1] == 1 && next[0] == 0 {
			seq = append(seq, vertical...)
			seq = append(seq, horizontal...)
		} else if dX < 0 {
			seq = append(seq, horizontal...)
			seq = append(seq, vertical...)
		} else if dX >= 0 {
			seq = append(seq, vertical...)
			seq = append(seq, horizontal...)
		}
		curr = next
		seq = append(seq, 'A')
	}
	return seq
}

func splitSeq(input []byte) [][]byte {
	output := [][]byte{}
	current := []byte{}
	for _, c := range input {
		current = append(current, c)

		if c == 'A' {
			output = append(output, current)
			current = []byte{}
		}
	}
	return output
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}
