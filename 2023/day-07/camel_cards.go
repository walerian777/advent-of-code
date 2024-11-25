package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(totalWinnings())
}

type Card byte

func cardStrength(c Card) int {
	cards := map[Card]int{
		'2': 2,
		'3': 3,
		'4': 4,
		'5': 5,
		'6': 6,
		'7': 7,
		'8': 8,
		'9': 9,
		'T': 10,
		'J': 11,
		'Q': 12,
		'K': 13,
		'A': 14,
	}
	return cards[c]
}

type Hand []Card

func handStrength(hand Hand) int {
	freq := make(map[Card]int)
	for _, c := range hand {
		freq[c] += 1
	}

	var otherPair bool
	var maybeFullHouse bool
	for _, v := range freq {
		if v == 5 {
			return 7
		}
		if v == 4 {
			return 6
		}
		if v == 3 {
			if otherPair {
				return 5
			}
			maybeFullHouse = true
		}
		if v == 2 {
			if maybeFullHouse {
				return 5
			}
			if otherPair {
				return 3
			}
			otherPair = true
		}
	}
	if maybeFullHouse {
		return 4
	}
	if otherPair {
		return 2
	}
	return 1
}

type Bid int

type HandBid struct {
	Hand
	Bid
}

func (h HandBid) String() string {
	return fmt.Sprintf("{Hand:%s, Bid:%d}", h.Hand, h.Bid)
}

func totalWinnings() int {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var hs []*HandBid

	for scanner.Scan() {
		line := strings.Fields(scanner.Text())
		hand, n := line[0], line[1]
		bid, err := strconv.Atoi(n)
		if err != nil {
			panic("cannot convert int")
		}
		hs = append(hs, &HandBid{Hand: Hand(hand), Bid: Bid(bid)})
	}

	sort.Slice(hs, func(i, j int) bool {
		strI := handStrength(hs[i].Hand)
		strJ := handStrength(hs[j].Hand)

		if strI < strJ {
			return true
		} else if strI > strJ {
			return false
		}

		for k := 0; k < 5; k++ {
			cStrI := cardStrength(hs[i].Hand[k])
			cStrJ := cardStrength(hs[j].Hand[k])
			if cStrI < cStrJ {
				return true
			} else if cStrI > cStrJ {
				return false
			}
		}
		return false
	})

	sum := 0
	for i, h := range hs {
		sum += (i + 1) * int(h.Bid)
	}

	return sum
}
