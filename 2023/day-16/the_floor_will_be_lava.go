package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fmt.Println(part1())
}

type Beam struct {
	Position  complex128
	Direction byte
}

func (b *Beam) NextPosition() complex128 {
	switch b.Direction {
	case '>':
		return b.Position + (0 + 1i)
	case '<':
		return b.Position + (0 - 1i)
	case '^':
		return b.Position + (-1 + 0i)
	case 'v':
		return b.Position + (1 + 0i)
	}
	panic("invalid direction")
}

func (b *Beam) SlashTurnDirection() byte {
	switch b.Direction {
	case '>':
		return '^'
	case '<':
		return 'v'
	case '^':
		return '>'
	case 'v':
		return '<'
	}
	panic("invalid direction")
}

func (b *Beam) BackslashTurnDirection() byte {
	switch b.Direction {
	case '>':
		return 'v'
	case '<':
		return '^'
	case '^':
		return '<'
	case 'v':
		return '>'
	}
	panic("invalid direction")
}

func (b *Beam) NextBeams(nextTile byte) []Beam {
	switch nextTile {
	case '.':
		return []Beam{Beam{b.Position, b.Direction}}
	case '/':
		nextDir := b.SlashTurnDirection()
		return []Beam{Beam{b.Position, nextDir}}
	case '\\':
		nextDir := b.BackslashTurnDirection()
		return []Beam{Beam{b.Position, nextDir}}
	case '-':
		if b.Direction == '<' || b.Direction == '>' {
			return []Beam{Beam{b.Position, b.Direction}}
		}
		return []Beam{Beam{b.Position, '<'}, Beam{b.Position, '>'}}
	case '|':
		if b.Direction == '^' || b.Direction == 'v' {
			return []Beam{Beam{b.Position, b.Direction}}
		}
		return []Beam{Beam{b.Position, '^'}, Beam{b.Position, 'v'}}
	default:
		panic("unknown tile type")
	}
}

func part1() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	grid := make(map[complex128]byte)
	curr := 0 + 0i
	for scanner.Scan() {
		l := scanner.Text()
		for j := range l {
			grid[curr+complex(0, float64(j))] = l[j]
		}
		curr += 1 + 0i
	}

	var beams []Beam
	beams = append(beams, (&Beam{0 + 0i, '>'}).NextBeams(grid[0+0i])...)
	energized := make(map[complex128]bool)
	visited := make(map[Beam]bool)

	for len(beams) > 0 {
		var nextBeams []Beam
		for _, b := range beams {
			energized[b.Position] = true
			if _, ok := visited[b]; ok {
				continue
			}
			visited[b] = true

			nextPos := b.NextPosition()
			nextTile, found := grid[nextPos]
			if !found {
				continue
			}
			nextBeams = append(nextBeams, (&Beam{nextPos, b.Direction}).NextBeams(nextTile)...)
		}
		beams = nextBeams
	}
	return len(energized)
}
