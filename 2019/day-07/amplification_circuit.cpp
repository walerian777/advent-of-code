// https://adventofcode.com/2019/day/7

#include <stdio.h>
#include <algorithm>

#define INPUT_SIZE 678
#define ADD_INSTRUCTION 1
#define MULTIPLY_INSTRUCTION 2
#define INPUT_INSTRUCTION 3
#define OUTPUT_INSTRUCTION 4
#define JUMP_IF_TRUE_INSTRUCTION 5
#define JUMP_IF_FALSE_INSTRUCTION 6
#define LESS_THAN_INSTRUCTION 7
#define EQUALS_INSTRUCTION 8
#define HALT_INSTRUCTION 99

struct instruction {
  int opcode;
  int firstParameterMode;
  int secondParameterMode;
  int resultParameterMode;
};

struct instruction parseInstruction(int rawInstruction) {
  struct instruction parsedInstruction = {
    .opcode = rawInstruction % 100,
    .firstParameterMode = rawInstruction / 100 % 10,
    .secondParameterMode = rawInstruction / 1000 % 10,
    .resultParameterMode = rawInstruction / 10000 % 10
  };

  return parsedInstruction;
}

int parameter(int mode, int instructionPointer, int *memory) {
  return mode > 0 ? instructionPointer : memory[instructionPointer];
}

int* inputInstructions(int firstInput, int secondInput) {
  static int instructions[2];

  instructions[0] = firstInput;
  instructions[1] = secondInput;

  return instructions;
}

int* copyMemory(int* memory) {
  static int workingMemory[INPUT_SIZE];

  for (int i = 0; i < INPUT_SIZE; i++) {
    workingMemory[i] = memory[i];
  }

  return workingMemory;
}

int executeProgram(int *memory, int *inputInstructions) {
  int instruction;
  int instructionPointer = 0;
  int opcode = memory[instructionPointer];

  struct instruction parsedInstruction;

  int firstParameter, secondParameter, resultParameter;

  int inputInstructionsPointer = 0;
  int outputSignal;

  while (instructionPointer < INPUT_SIZE) {
    instruction = memory[instructionPointer];
    parsedInstruction = parseInstruction(instruction);

    opcode = parsedInstruction.opcode;
    firstParameter = parameter(parsedInstruction.firstParameterMode, instructionPointer + 1, memory);
    secondParameter = parameter(parsedInstruction.secondParameterMode, instructionPointer + 2, memory);
    resultParameter = parameter(parsedInstruction.resultParameterMode, instructionPointer + 3, memory);

    if (opcode == ADD_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] + memory[secondParameter];
      instructionPointer += 4;
    } else if (opcode == MULTIPLY_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] * memory[secondParameter];
      instructionPointer += 4;
    } else if (opcode == INPUT_INSTRUCTION) {
      memory[firstParameter] = inputInstructions[inputInstructionsPointer++];
      instructionPointer += 2;
    } else if (opcode == OUTPUT_INSTRUCTION) {
      outputSignal = memory[firstParameter];
      instructionPointer += 2;

      return outputSignal;
    } else if (opcode == JUMP_IF_TRUE_INSTRUCTION) {
      if (memory[firstParameter] != 0) {
        instructionPointer = memory[secondParameter];
      } else {
        instructionPointer += 3;
      };
    } else if (opcode == JUMP_IF_TRUE_INSTRUCTION) {
      if (memory[firstParameter] == 0) {
        instructionPointer = memory[secondParameter];
      } else {
        instructionPointer += 3;
      };
    } else if (opcode == LESS_THAN_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] < memory[secondParameter];
      instructionPointer += 4;
    } else if (opcode == EQUALS_INSTRUCTION) {
      memory[resultParameter] = memory[firstParameter] == memory[secondParameter];
      instructionPointer += 4;
    } else if (opcode == HALT_INSTRUCTION) {
      return 0;
    };
  }

  return 0;
}

int main() {
  FILE *inputFile;
  inputFile = fopen("input", "r");

  int memory[INPUT_SIZE];

  for (int i = 0; i < INPUT_SIZE; i++) {
    fscanf(inputFile, "%d,", &memory[i]);
  }

  // Part 1
  int highestSignal = 0;
  int outputSignal = 0;
  int settingSequence[5] = {0, 1, 2, 3, 4};

  do {
    outputSignal = executeProgram(copyMemory(memory), inputInstructions(settingSequence[0], 0));
    outputSignal = executeProgram(copyMemory(memory), inputInstructions(settingSequence[1], outputSignal));
    outputSignal = executeProgram(copyMemory(memory), inputInstructions(settingSequence[2], outputSignal));
    outputSignal = executeProgram(copyMemory(memory), inputInstructions(settingSequence[3], outputSignal));
    outputSignal = executeProgram(copyMemory(memory), inputInstructions(settingSequence[4], outputSignal));

    if(outputSignal > highestSignal) {
      highestSignal = outputSignal;
    }
  } while (std::next_permutation(settingSequence, settingSequence + 5));

  printf("Highest signal: %d", highestSignal);

  // Part 2
  highestSignal = 0;
  int feedbackSettingSequence[5] = {5, 6, 7, 8, 9};

  do {
    outputSignal = 0;

    int *workingMemory[5] = {copyMemory(memory), copyMemory(memory), copyMemory(memory), copyMemory(memory), copyMemory(memory)};

    while(true) {
      outputSignal = executeProgram(workingMemory[0], inputInstructions(feedbackSettingSequence[0], outputSignal));
      outputSignal = executeProgram(workingMemory[1], inputInstructions(feedbackSettingSequence[1], outputSignal));
      outputSignal = executeProgram(workingMemory[2], inputInstructions(feedbackSettingSequence[2], outputSignal));
      outputSignal = executeProgram(workingMemory[3], inputInstructions(feedbackSettingSequence[3], outputSignal));
      outputSignal = executeProgram(workingMemory[4], inputInstructions(feedbackSettingSequence[4], outputSignal));

      if (outputSignal == 0) break;
    }

    if(outputSignal > highestSignal) {
      highestSignal = outputSignal;
    }
  } while (std::next_permutation(feedbackSettingSequence, feedbackSettingSequence + 5));

  printf("Highest signal with feedback loop: %d", highestSignal);

  return 0;
}

