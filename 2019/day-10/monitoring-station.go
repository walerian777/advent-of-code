// https://adventofcode.com/2019/day/10

package main

import (
  "fmt"
  "io/ioutil"
  "strings"
  "math"
  "sort"
)

func main() {
  input, err := ioutil.ReadFile("input")
  check(err)

  asteroids := strings.Split(string(input), "\n")
  asteroids = asteroids[:len(asteroids) - 1]

  rows_count := len(asteroids)
  columns_count := len(asteroids[0])

  max_detected_asteroids := 0
  detected_asteroids := map[Coordinate]bool{}
  station_row := 0
  station_column := 0

  for i := 0; i < rows_count; i++ {
    for j := 0; j < columns_count; j++ {
      if asteroids[i][j] != '#' {
        break
      }

      visited_asteroids := map[Coordinate]bool{}
      for row := 0; row < rows_count; row++ {
        for column := 0; column < columns_count; column++ {
          if asteroids[row][column] == '#' && (row != i || column != j) {
            row_diff := row - i
            column_diff := column - j
            divisor := gcd(row_diff, column_diff)

            coordinates := Coordinate{row_diff / divisor, column_diff / divisor}

            visited_asteroids[coordinates] = true
          }
        }
      }

      if len(visited_asteroids) > max_detected_asteroids {
        max_detected_asteroids = len(visited_asteroids)
        detected_asteroids = visited_asteroids
        station_row = i
        station_column = j
      }
    }
  }

  fmt.Println(max_detected_asteroids)

  var candidates []Candidate
  for coordinate := range detected_asteroids {
    key := atan(coordinate.a, coordinate.b)
    candidate := Candidate{key, coordinate}

    candidates = append(candidates, candidate)
  }

  sort.Slice(candidates, func(i, j int) bool {
    return candidates[i].key < candidates[j].key
  })


  winner_row := candidates[199].coordinate.a
  winner_column := candidates[199].coordinate.b

  row := station_row - winner_row
  column := station_column + winner_column

  fmt.Println(column * 100 + row)
}

type Coordinate struct {
  a int
  b int
}

type Candidate struct {
  key float64
  coordinate Coordinate
}

func abs(a int) int {
  if a < 0 {
    return a * -1
  } else {
    return a
  }
}

func gcd(a, b int) int {
  if a == 0 {
    return abs(b)
  } else {
    return gcd(b % a, a)
  }
}

func atan(a, b int) float64 {
  value := math.Atan2(float64(a), float64(b))

  if value > math.Pi / 2.0 {
    return value - math.Pi / 2.0
  } else {
    return value
  }
}

func check(e error) {
  if e != nil {
    panic(e)
  }
}
