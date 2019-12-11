// https://adventofcode.com/2019/day/10

package main

import (
  "fmt"
  "io/ioutil"
  "strings"
)

func main() {
  input, err := ioutil.ReadFile("input")
  check(err)

  asteroids := strings.Split(string(input), "\n")
  asteroids = asteroids[:len(asteroids) - 1]

  rows_count := len(asteroids)
  columns_count := len(asteroids[0])

  max_detected_asteroids := 0

  for i := 0; i < rows_count; i++ {
    for j := 0; j < columns_count; j++ {
      if asteroids[i][j] != '#' {
        break
      }

      visited_asteroids := map[Coordinates]bool{}
      for row := 0; row < rows_count; row++ {
        for column := 0; column < columns_count; column++ {
          if asteroids[row][column] == '#' && (row != i || column != j) {
            row_diff := row - i
            column_diff := column - j
            divisor := gcd(row_diff, column_diff)

            coordinates := Coordinates{row_diff / divisor, column_diff / divisor}

            visited_asteroids[coordinates] = true
          }
        }
      }

      if len(visited_asteroids) > max_detected_asteroids {
        max_detected_asteroids = len(visited_asteroids)
      }
    }
  }

  fmt.Println(max_detected_asteroids)
}

type Coordinates struct {
  a, b interface{}
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

func check(e error) {
  if e != nil {
    panic(e)
  }
}
