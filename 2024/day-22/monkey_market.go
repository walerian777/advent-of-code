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

	var secrets []int
	for scanner.Scan() {
		secrets = append(secrets, MustAtoi(scanner.Text()))
	}

	var sum int
	for _, s := range secrets {
		x := s
		for range 2000 {
			x = NextSecret(x)
		}
		sum += x
	}

	return sum
}

func NextSecret(secret int) int {
	newSecret := secret
	r1 := newSecret << 6
	newSecret = Mix(newSecret, r1)
	newSecret = Prune(newSecret)

	r2 := newSecret >> 5
	newSecret = Mix(newSecret, r2)
	newSecret = Prune(newSecret)

	r3 := newSecret << 11
	newSecret = Mix(newSecret, r3)
	newSecret = Prune(newSecret)
	return newSecret
}

func Mix(secret, value int) int {
	return secret ^ value
}

func Prune(secret int) int {
	return secret & 16777215
}

func MustAtoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic("strconv")
	}

	return i
}
