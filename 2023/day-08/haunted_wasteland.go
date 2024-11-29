package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fmt.Println(stepsToZzz())
	fmt.Println(ghostStepsToZzz())
}

type Instructions []byte

type Node struct {
	Id     string
	Left   *Node
	Right  *Node
	Ending bool
}

func NewNode(id string) *Node {
	return &Node{Id: id, Ending: id[2] == 'Z'}
}

func (n *Node) String() string {
	var left, right string
	if n.Left != nil {
		left = n.Left.Id
	}

	if n.Right != nil {
		right = n.Right.Id
	}
	return fmt.Sprintf("%s = (%s, %s)\n", n.Id, left, right)
}

func stepsToZzz() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	// First line includes instructions
	scanner.Scan()
	instructions := Instructions(scanner.Text())

	// Second line is empty
	scanner.Scan()

	nodes := make(map[string]*Node)

	for scanner.Scan() {
		line := scanner.Text()
		id, left, right := line[0:3], line[7:10], line[12:15]

		node, ok := nodes[id]
		if !ok {
			node = &Node{Id: id}
			nodes[id] = node
		}

		leftNode, ok := nodes[left]
		if !ok {
			leftNode = &Node{Id: left}
			nodes[left] = leftNode
		}
		node.Left = leftNode

		rightNode, ok := nodes[right]
		if !ok {
			rightNode = &Node{Id: right}
			nodes[right] = rightNode
		}
		node.Right = rightNode

	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	curr := nodes["AAA"]
	var steps int
	for {
		if curr.Id == "ZZZ" {
			break
		}
		for _, instruction := range instructions {
			steps++
			if instruction == 'L' {
				curr = curr.Left
			} else if instruction == 'R' {
				curr = curr.Right

			} else {
				panic("Invalid instruction!")
			}
		}

	}

	return steps
}

func ghostStepsToZzz() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	// First line includes instructions
	scanner.Scan()
	instructions := Instructions(scanner.Text())

	// Second line is empty
	scanner.Scan()

	nodes := make(map[string]*Node)
	var currNodes []*Node

	for scanner.Scan() {
		line := scanner.Text()
		id, left, right := line[0:3], line[7:10], line[12:15]

		node, ok := nodes[id]
		if !ok {
			node = NewNode(id)
			nodes[id] = node
		}

		leftNode, ok := nodes[left]
		if !ok {
			leftNode = NewNode(left)
			nodes[left] = leftNode
		}
		node.Left = leftNode

		rightNode, ok := nodes[right]
		if !ok {
			rightNode = NewNode(right)
			nodes[right] = rightNode
		}
		node.Right = rightNode

		if node.Id[2] == 'A' {
			currNodes = append(currNodes, node)
		}

	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var stepsList []int
	for _, currNode := range currNodes {
		var steps int
		for {
			if currNode.Ending {
				stepsList = append(stepsList, steps)
				break
			}
			for _, instruction := range instructions {
				steps++
				if instruction == 'L' {
					currNode = currNode.Left
				} else if instruction == 'R' {
					currNode = currNode.Right
				} else {
					panic("Invalid instruction!")
				}
			}

		}
	}

	res := 1
	for _, steps := range stepsList {
		res = lcm(res, steps)
	}

	return res
}

func gcd(a, b int) int {
	for a != b {
		if a > b {
			a -= b
		} else {
			b -= a
		}
	}
	return a
}

func lcm(a, b int) int {
	return a / gcd(a, b) * b
}
