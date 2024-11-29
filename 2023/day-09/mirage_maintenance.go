package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(sumOfValues())
	fmt.Println(sumOfBackwardsValues())
}

func sumOfValues() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var histories [][]int
	for scanner.Scan() {
		values := strings.Fields(scanner.Text())
		var history []int
		for _, v := range values {
			n, err := strconv.Atoi(v)
			if err != nil {
				panic("cannot convert to int")
			}
			history = append(history, n)
		}
		histories = append(histories, history)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var sum int

	for _, history := range histories {
		lastDiff := history[len(history)-1]
		for sumAll(&history) != 0 {
			var diff int
			var nextHistory []int
			for i := 1; i < len(history); i++ {
				diff = history[i] - history[i-1]
				nextHistory = append(nextHistory, diff)

			}
			history = nextHistory
			lastDiff += diff
		}
		sum += lastDiff

	}

	return sum
}

func sumOfBackwardsValues() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var histories [][]int
	for scanner.Scan() {
		values := strings.Fields(scanner.Text())
		var history []int
		for _, v := range values {
			n, err := strconv.Atoi(v)
			if err != nil {
				panic("cannot convert to int")
			}
			history = append(history, n)
		}
		histories = append(histories, history)
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var sum int

	for _, history := range histories {
		slices.Reverse(history)
		lastDiff := history[len(history)-1]
		for sumAll(&history) != 0 {
			var diff int
			var nextHistory []int
			for i := 1; i < len(history); i++ {
				diff = history[i] - history[i-1]
				nextHistory = append(nextHistory, diff)

			}
			history = nextHistory
			lastDiff += diff
		}
		sum += lastDiff
	}

	return sum

}

func sumAll(ns *[]int) int {
	var s int
	for _, n := range *ns {
		s += n
	}
	return s
}
