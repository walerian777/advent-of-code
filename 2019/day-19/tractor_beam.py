# https://adventofcode.com/2019/day/19

class IntcodeComputer:
    def __init__(self, program_input):
        self.memory = program_input.copy() + [0] * 5000
        self.instruction_pointer = 0
        self.relative_base = 0
        self.next_input = 0
        self.input_queue = []
        self.output_queue = []

    def fetch_param(self, pointer, mode, accessisble=False):
        if mode == 0:
            param = pointer
        elif mode == 1:
            return pointer
        elif mode == 2:
            param = self.relative_base + pointer

        if accessisble:
            return param
        else:
            return self.memory[param]

    def fetch_input(self):
        value = self.input_queue[self.next_input]
        self.next_input += 1
        return value

    def execute(self, inputValue=None):
        if inputValue is not None:
            self.input_queue += inputValue

        while self.instruction_pointer < len(self.memory):
            instruction = self.memory[self.instruction_pointer]

            opcode = instruction % 100
            first_param_mode = (instruction // 100) % 10
            second_param_mode = (instruction // 1000) % 10
            third_param_mode = (instruction // 10000) % 10

            if opcode == 1:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                third_param = self.fetch_param(self.memory[self.instruction_pointer + 3], third_param_mode, True)
                self.instruction_pointer += 4
                self.memory[third_param] = first_param + second_param
            elif opcode == 2:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                third_param = self.fetch_param(self.memory[self.instruction_pointer + 3], third_param_mode, True)
                self.instruction_pointer += 4
                self.memory[third_param] = first_param * second_param
            elif opcode == 3:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode, True)
                self.instruction_pointer += 2
                current_input = self.fetch_input()
                if current_input is None:
                    return
                self.memory[first_param] = current_input
            elif opcode == 4:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                self.instruction_pointer += 2
                self.output_queue.append(first_param)
            elif opcode == 5:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                self.instruction_pointer += 3
                if first_param != 0:
                    self.instruction_pointer = second_param
            elif opcode == 6:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                self.instruction_pointer += 3
                if first_param == 0:
                    self.instruction_pointer = second_param
            elif opcode == 7:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                third_param = self.fetch_param(self.memory[self.instruction_pointer + 3], third_param_mode, True)
                self.instruction_pointer += 4
                self.memory[third_param] = int(first_param < second_param)
            elif opcode == 8:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                second_param = self.fetch_param(self.memory[self.instruction_pointer + 2], second_param_mode)
                third_param = self.fetch_param(self.memory[self.instruction_pointer + 3], third_param_mode, True)
                self.instruction_pointer += 4
                self.memory[third_param] = int(first_param == second_param)
            elif opcode == 9:
                first_param = self.fetch_param(self.memory[self.instruction_pointer + 1], first_param_mode)
                self.instruction_pointer += 2
                self.relative_base += first_param
            elif opcode == 99:
                break

        return

def load_input():
    with open('input') as inp:
        input_data = inp.read().strip()

    return [int(i) for i in input_data.split(',')]

def fetch_coordinates(x, y, memory):
    computer = IntcodeComputer(memory)
    computer.execute([x, y])
    return computer.output_queue.pop()

def calculate_affected_points(memory):
    affected_points = 0
    for x in range(50):
        for y in range(50):
            affected_points += fetch_coordinates(x, y, memory)

    return affected_points


initial_memory = load_input()
print(calculate_affected_points(initial_memory))
