# https://adventofcode.com/2019/day/21

class IntcodeComputer:
    def __init__(self, memory_input):
        self.memory = memory_input.copy() + [0] * 5000
        self.instruction_pointer = 0
        self.relative_base = 0
        self.next_input = 0
        self.input_queue = []
        self.output_queue = []
        self.idle = False

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
        if len(self.input_queue):
            value = self.input_queue[self.next_input]
            self.next_input += 1
            return value
        return None

    def fetch_output(self):
        if len(self.input_queue):
            return self.output_queue.pop()
        return None

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
                self.idle = True
                break

        return

def load_input():
    with open('input') as inp:
        input_data = inp.read().strip()
    return [int(i) for i in input_data.split(',')]

def prepare_instructions():
    instructions = [
            'NOT C J',
            'NOT A T',
            'OR T J',
            'AND D J',
            'WALK'
            ]
    return ''.join(instruction + '\n' for instruction in instructions)

def calculate_hull_damage(memory, instructions):
    computer = IntcodeComputer(memory)
    output = 0

    while output != 10:
      computer.execute()
      computer.fetch_output()

    characters = [ord(character) for character in instructions]

    computer.execute(characters)

    while not computer.idle:
        output = computer.fetch_output()
        if output > 255:
            return output
        computer.execute()

    return computer.fetch_output()

initial_memory = load_input()
instructions = prepare_instructions()
print(calculate_hull_damage(initial_memory, instructions))
