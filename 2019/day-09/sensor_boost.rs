// https://adventofcode.com/2019/day/9

fn main() {
    let data = std::fs::read_to_string("input")
        .expect("Something went wrong reading the file");

    let input = 1;
    let mut output = Vec::new();
    let mut memory: Vec<i64> = data.trim_end().split(",").map(|x| x.parse().unwrap()).collect();
    for _i in 0..1000 { memory.push(0) }

    let mut instruction_pointer: i64 = 0;
    let mut relative_base: i64 = 0;

    while instruction_pointer < memory.len() as i64 {
        let instruction = memory[instruction_pointer as usize];
        let opcode = instruction % 100;
        let first_param_mode = instruction / 100 % 10;
        let second_param_mode = instruction / 1000 % 10;
        let result_param_mode = instruction / 10000 % 10;

        let first_param = fetch_parameter(first_param_mode, instruction_pointer + 1, relative_base, &memory);
        let second_param = fetch_parameter(second_param_mode, instruction_pointer + 2, relative_base, &memory);
        let result_param = fetch_parameter(result_param_mode, instruction_pointer + 3, relative_base, &memory);

        match opcode {
            1 => {
                memory[result_param as usize] = memory[first_param as usize] + memory[second_param as usize];
                instruction_pointer += 4
            },
            2 => {
                memory[result_param as usize] = memory[first_param as usize] * memory[second_param as usize];
                instruction_pointer += 4
            },
            3 => {
                memory[first_param as usize] = input;
                instruction_pointer += 2;
            },
            4 => {
                output.push(memory[first_param as usize]);
                instruction_pointer += 2;
            },
            5 => {
                // getValue(a) != 0 ? i = getValue(b) : i+=2
                if memory[first_param as usize] != 0 {
                    instruction_pointer = memory[second_param as usize];
                } else {
                    instruction_pointer += 3;
                }
            },
            6 => {
                // getValue(a)==0 ? i = getValue(b) : i+=2;
                if memory[first_param as usize] == 0 {
                    instruction_pointer = memory[second_param as usize];
                } else {
                    instruction_pointer += 3;
                }
            },
            7 => {
                memory[result_param as usize] = (memory[first_param as usize] < memory[second_param as usize]) as i64;
                instruction_pointer += 4;
            },
            8 => {
                memory[result_param as usize] = (memory[first_param as usize] == memory[second_param as usize]) as i64;
                instruction_pointer += 4;
            },
            9 => {
                relative_base += memory[first_param as usize];
                instruction_pointer += 2;
            },
            99 => {
                break;
            }
            _ => println!("Oops")
        }
    }

    println!("{:?}", output);
}

fn fetch_parameter(mode: i64, instruction_pointer: i64, relative_base: i64, memory: &Vec<i64>) -> i64 {
    return match mode {
        0 => memory[instruction_pointer as usize],
        1 => instruction_pointer,
        2 => relative_base + memory[instruction_pointer as usize],
        _ => 0
    }
}
