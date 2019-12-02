// https://adventofcode.com/2019/day/1

#include <stdio.h>

#define INPUT_SIZE 137
#define ADD_CODE 1
#define MULTIPLY_CODE 2
#define HALT_CODE 99

int main() {
  FILE *inputFile;
  inputFile = fopen("input", "r");

  int codes[INPUT_SIZE];

  for (int i = 0; i < INPUT_SIZE; i++) {
    fscanf(inputFile, "%d,", &codes[i]);
  }

  int firstInputPosition, secondInputPosition, resultPosition, currentPosition = 0;
  int currentOptcode = currentOptcode = codes[currentPosition];

  while (currentOptcode != HALT_CODE) {
    currentOptcode = codes[currentPosition];
    firstInputPosition = codes[currentPosition + 1];
    secondInputPosition = codes[currentPosition + 2];
    resultPosition = codes[currentPosition + 3];

    if (currentOptcode == ADD_CODE) {
      codes[resultPosition] = codes[firstInputPosition] + codes[secondInputPosition];
    } else if (currentOptcode == MULTIPLY_CODE) {
      codes[resultPosition] = codes[firstInputPosition] * codes[secondInputPosition];
    };

    currentPosition += 4;
  }

  printf("%d\n", codes[0]);
  return 0;
}
