// https://adventofcode.com/2019/day/5

#include <stdio.h>

#define INPUT_SIZE 678
#define ADD_INSTRUCTION 1
#define MULTIPLY_INSTRUCTION 2
#define INPUT_INSTRUCTION 3
#define OUTPUT_INSTRUCTION 4
#define HALT_INSTRUCTION 99

#define POSITION_MODE 0
#define IMMEDIATE_MODE 1

int main() {
  FILE *inputFile;
  inputFile = fopen("input", "r");

  int memory[INPUT_SIZE];

  for (int i = 0; i < INPUT_SIZE; i++) {
    fscanf(inputFile, "%d,", &memory[i]);
  }

  int instruction, instructionPointer, instructionOffset, opcode;
  int firstParameterMode, secondParameterMode, resultParameterMode;
  int firstParameter, secondParameter, resultParameter;
  int input, output;
  
  instructionPointer = 0;
  opcode = memory[instructionPointer];

  while (opcode != HALT_INSTRUCTION) {
    instruction = memory[instructionPointer];

    opcode = instruction % 100;
    firstParameterMode = instruction / 100 % 10;
    secondParameterMode = instruction / 1000 % 10;
    resultParameterMode = instruction / 10000 % 10;

    firstParameter = firstParameterMode > 0 ? instructionPointer + 1 : memory[instructionPointer + 1];
    secondParameter = secondParameterMode > 0 ? instructionPointer + 2 : memory[instructionPointer + 2];
    resultParameter = resultParameterMode > 0 ? instructionPointer + 3 : memory[instructionPointer + 3];

    if (opcode == ADD_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] + memory[secondParameter];
      instructionOffset = 4;
    } else if (opcode == MULTIPLY_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] * memory[secondParameter];
      instructionOffset = 4;
    } else if (opcode == INPUT_INSTRUCTION) {
      printf("Provide the ID of the system to test: ");
      fscanf(stdin, "%d", &input);
      memory[firstParameter] = input;
      instructionOffset = 2;
    } else if (opcode == OUTPUT_INSTRUCTION) {
      output = memory[firstParameter];
      printf("Diagnostic code: %d\n", output);
      instructionOffset = 2;
    };

    instructionPointer += instructionOffset;
  }

  return 0;
}
