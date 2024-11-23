package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(lowestLocationNumber())
	fmt.Println(lowestRangeLocationNumber())
}

type SeedId int
type SoilId int
type FertilizerID int
type WaterId int
type LightId int
type TemperatureId int
type HumidityId int
type LocationId int

type SeedMap[In ~int, Out ~int] struct {
	Sources      []In
	Destinations []Out
	Lengths      []int
}

func (m *SeedMap[In, Out]) translate(id In) Out {
	for i := range m.Lengths {
		if m.Sources[i] <= id && m.Sources[i]+In(m.Lengths[i]) >= id {
			return Out(id) + (m.Destinations[i] - Out(m.Sources[i]))
		}
	}
	return Out(id)
}

func (m *SeedMap[In, Out]) append(source In, destination Out, length int) *SeedMap[In, Out] {
	m.Destinations = append(m.Destinations, destination)
	m.Sources = append(m.Sources, source)
	m.Lengths = append(m.Lengths, length)
	return m
}

func lowestLocationNumber() LocationId {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	seedsLine := scanner.Text()
	parts := strings.Split(seedsLine, ":")
	if len(parts) < 2 {
		panic("invalid input")
	}
	var seeds []SeedId
	for _, s := range strings.Fields(parts[1]) {
		n, err := strconv.Atoi(s)
		if err != nil {
			panic("cannot conver to int")
		}
		seeds = append(seeds, SeedId(n))
	}

	i := 0
	m1 := &SeedMap[SeedId, SoilId]{}
	m2 := &SeedMap[SoilId, FertilizerID]{}
	m3 := &SeedMap[FertilizerID, WaterId]{}
	m4 := &SeedMap[WaterId, LightId]{}
	m5 := &SeedMap[LightId, TemperatureId]{}
	m6 := &SeedMap[TemperatureId, HumidityId]{}
	m7 := &SeedMap[HumidityId, LocationId]{}
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		if line[len(line)-1] == ':' {
			i++
			continue
		}
		ns := strings.Fields(line)
		if len(ns) != 3 {
			panic("expected three numbers")
		}
		n1, err := strconv.Atoi(ns[0])
		if err != nil {
			panic("cannot conver to int")
		}
		n2, err := strconv.Atoi(ns[1])
		if err != nil {
			panic("cannot conver to int")
		}
		n3, err := strconv.Atoi(ns[2])
		if err != nil {
			panic("cannot conver to int")
		}

		switch i {
		case 1:
			m1.append(SeedId(n2), SoilId(n1), n3)
		case 2:
			m2.append(SoilId(n2), FertilizerID(n1), n3)
		case 3:
			m3.append(FertilizerID(n2), WaterId(n1), n3)
		case 4:
			m4.append(WaterId(n2), LightId(n1), n3)
		case 5:
			m5.append(LightId(n2), TemperatureId(n1), n3)
		case 6:
			m6.append(TemperatureId(n2), HumidityId(n1), n3)
		case 7:
			m7.append(HumidityId(n2), LocationId(n1), n3)
		default:
			panic("expected 7 cases!")
		}

	}
	var minL LocationId
	for i, s := range seeds {
		l := m7.translate(m6.translate(m5.translate(m4.translate(m3.translate(m2.translate(m1.translate(s)))))))
		if i == 0 {
			minL = l
		} else {
			if l < minL {
				minL = l
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner errors")
	}

	return minL
}

type SeedRange struct {
	Start  SeedId
	Length int
}

func lowestRangeLocationNumber() LocationId {
	file, err := os.Open("input")
	if err != nil {
		panic("cannot open file")
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	seedsLine := scanner.Text()
	parts := strings.Split(seedsLine, ":")
	if len(parts) < 2 {
		panic("invalid input")
	}
	seeds := strings.Fields(parts[1])
	var seedRanges []*SeedRange
	for i := 0; i < len(seeds); i += 2 {
		s, err := strconv.Atoi(seeds[i])
		if err != nil {
			panic("cannot conver to int")
		}
		l, err := strconv.Atoi(seeds[i+1])
		if err != nil {
			panic("cannot conver to int")
		}
		seedRanges = append(seedRanges, &SeedRange{Start: SeedId(s), Length: l})
	}

	i := 0
	m1 := &SeedMap[SeedId, SoilId]{}
	m2 := &SeedMap[SoilId, FertilizerID]{}
	m3 := &SeedMap[FertilizerID, WaterId]{}
	m4 := &SeedMap[WaterId, LightId]{}
	m5 := &SeedMap[LightId, TemperatureId]{}
	m6 := &SeedMap[TemperatureId, HumidityId]{}
	m7 := &SeedMap[HumidityId, LocationId]{}
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		if line[len(line)-1] == ':' {
			i++
			continue
		}
		ns := strings.Fields(line)
		if len(ns) != 3 {
			panic("expected three numbers")
		}
		n1, err := strconv.Atoi(ns[0])
		if err != nil {
			panic("cannot conver to int")
		}
		n2, err := strconv.Atoi(ns[1])
		if err != nil {
			panic("cannot conver to int")
		}
		n3, err := strconv.Atoi(ns[2])
		if err != nil {
			panic("cannot conver to int")
		}

		switch i {
		case 1:
			m1.append(SeedId(n2), SoilId(n1), n3)
		case 2:
			m2.append(SoilId(n2), FertilizerID(n1), n3)
		case 3:
			m3.append(FertilizerID(n2), WaterId(n1), n3)
		case 4:
			m4.append(WaterId(n2), LightId(n1), n3)
		case 5:
			m5.append(LightId(n2), TemperatureId(n1), n3)
		case 6:
			m6.append(TemperatureId(n2), HumidityId(n1), n3)
		case 7:
			m7.append(HumidityId(n2), LocationId(n1), n3)
		default:
			panic("expected 7 cases!")
		}

	}
	var minL LocationId
	var firstL bool
	for _, sr := range seedRanges {
		for j := 0; j < sr.Length; j++ {
			l := m7.translate(m6.translate(m5.translate(m4.translate(m3.translate(m2.translate(m1.translate(sr.Start + SeedId(j))))))))
			if !firstL {
				minL = l
				firstL = true
			} else {
				if l < minL {
					minL = l
				}
			}
		}
	}

	if err := scanner.Err(); err != nil {
		panic("scanner errors")
	}

	return minL
}
