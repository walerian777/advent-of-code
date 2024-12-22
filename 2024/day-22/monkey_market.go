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

func part2() int {
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

	deltas := make(map[[4]int]int)
	for _, s := range secrets {
		x := s
		ps := []int{x}
		ds := []int{0}
		for range 2000 {
			y := NextSecret(x)
			ps = append(ps, y%10)
			ds = append(ds, y%10-x%10)
			x = y
		}

		localDeltas := make(map[[4]int]int)
		for i := 1; i+3 < len(ds); i++ {
			k := [4]int{ds[i], ds[i+1], ds[i+2], ds[i+3]}
			if _, ok := localDeltas[k]; !ok {
				localDeltas[k] = ps[i+3]
				deltas[k] += ps[i+3]
			}
		}
	}
	maxPrice := -1
	for _, v := range deltas {
		if v > maxPrice {
			maxPrice = v
		}
	}
	return maxPrice
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
