package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fmt.Println(stepsToZzz())
}

type Instructions []byte

type Node struct {
	Id    string
	Left  *Node
	Right *Node
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

	if err := scanner.Err(); err != nil {
		fmt.Errorf("Scanner error", err)
	}

	return steps
}
