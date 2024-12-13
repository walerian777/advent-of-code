package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

func main() {
	fmt.Println(part1())
	fmt.Println(part2())
}

type Config struct {
	a, b, p [2]int64
}

func (c Config) String() string {
	return fmt.Sprintf("[A]: X+%d, Y+%d | [B]: X+%d, Y+%d | X=%d, Y=%d", c.a[0], c.a[1], c.b[0], c.b[1], c.p[0], c.p[1])
}

func (c *Config) Tokens() int64 {
	det := c.a[0]*c.b[1] - c.a[1]*c.b[0]
	if det == 0 {
		return 0
	}
	a := (c.p[0]*c.b[1] - c.p[1]*c.b[0]) / det
	b := (c.a[0]*c.p[1] - c.a[1]*c.p[0]) / det

	if a*c.a[0]+b*c.b[0] == c.p[0] && a*c.a[1]+b*c.b[1] == c.p[1] {
		return 3*a + b
	}
	return 0
}

func part1() int64 {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	reButton := regexp.MustCompile(`Button.+X\+(\d+), Y\+(\d+)`)
	rePrize := regexp.MustCompile(`Prize.+X=(\d+), Y=(\d+)`)

	var configs []Config
	currentCfg := Config{}
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			configs = append(configs, currentCfg)
			currentCfg = Config{}
			continue
		}
		switch line[7] {
		case 'A':
			a := reButton.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(a[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(a[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.a = [2]int64{int64(x), int64(y)}
		case 'B':
			b := reButton.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(b[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(b[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.b = [2]int64{int64(x), int64(y)}
		case 'X':
			b := rePrize.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(b[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(b[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.p = [2]int64{int64(x), int64(y)}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var sum int64
	for _, cfg := range configs {
		sum += cfg.Tokens()
	}
	return sum
}

func part2() int64 {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	reButton := regexp.MustCompile(`Button.+X\+(\d+), Y\+(\d+)`)
	rePrize := regexp.MustCompile(`Prize.+X=(\d+), Y=(\d+)`)

	var configs []Config
	currentCfg := Config{}
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			configs = append(configs, currentCfg)
			currentCfg = Config{}
			continue
		}
		switch line[7] {
		case 'A':
			a := reButton.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(a[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(a[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.a = [2]int64{int64(x), int64(y)}
		case 'B':
			b := reButton.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(b[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(b[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.b = [2]int64{int64(x), int64(y)}
		case 'X':
			b := rePrize.FindAllStringSubmatch(line, 1)[0]
			x, err := strconv.Atoi(b[1])
			if err != nil {
				panic("atoi")
			}
			y, err := strconv.Atoi(b[2])
			if err != nil {
				panic("atoi")
			}
			currentCfg.p = [2]int64{int64(x) + 10000000000000, int64(y) + 10000000000000}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner error")
	}

	var sum int64
	for _, cfg := range configs {
		sum += cfg.Tokens()
	}
	return sum
}
