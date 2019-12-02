// https://adventofcode.com/2019/day/2

#include <stdio.h>

#define INPUT_SIZE 137
#define ADD_INSTRUCTION 1
#define MULTIPLY_INSTRUCTION 2
#define HALT_INSTRUCTION 99

#define NOUN_INDEX 1
#define VERB_INDEX 2
#define EXPECTED_RESULT 19690720

int main() {
  FILE *inputFile;
  inputFile = fopen("input", "r");

  int memory[INPUT_SIZE];
  int workingMemory[INPUT_SIZE];

  for (int i = 0; i < INPUT_SIZE; i++) {
    fscanf(inputFile, "%d,", &memory[i]);
  }

  int opcode, instructionPointer;
  int firstParameterAddress, secondParameterAddress, resultAddress;
  int noun, verb;

  for (noun = 0; noun < 100; noun++) {
    for (verb = 0; verb < 100; verb++) {
      for(int i = 0; i < INPUT_SIZE; i++) {
        workingMemory[i] = memory[i];
      }

      workingMemory[NOUN_INDEX] = noun;
      workingMemory[VERB_INDEX] = verb;

      instructionPointer = 0;
      opcode = workingMemory[instructionPointer];

      while (opcode != HALT_INSTRUCTION) {
        opcode = workingMemory[instructionPointer];
        firstParameterAddress = workingMemory[instructionPointer + 1];
        secondParameterAddress = workingMemory[instructionPointer + 2];
        resultAddress = workingMemory[instructionPointer + 3];

        if (opcode == ADD_INSTRUCTION) {
          workingMemory[resultAddress] = workingMemory[firstParameterAddress] + workingMemory[secondParameterAddress];
        } else if (opcode == MULTIPLY_INSTRUCTION) {
          workingMemory[resultAddress] = workingMemory[firstParameterAddress] * workingMemory[secondParameterAddress];
        };

        instructionPointer += 4;
      }

      if (workingMemory[0] == EXPECTED_RESULT) {
        printf("%d\n%d\n", noun, verb);
        return 0;
      }
    }
  }

  printf("Not found!\n");
  return 0;
}
