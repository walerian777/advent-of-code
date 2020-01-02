// https://adventofcode.com/2015/day/6

use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let instructions: Vec<&str> = input.trim_end().split("\n").collect();

    let mut lights = HashMap::new();

    for x in 0..=999 {
        for y in 0..=999 {
            lights.insert((x, y), false);
        }
    }

    for instruction in instructions {
        if instruction.starts_with("turn on") {
            let coordinates = parse_instruction(instruction, "turn on");
            execute_command(&mut lights, turn_on, &coordinates[0], &coordinates[1])
        } else if instruction.starts_with("turn off") {
            let coordinates = parse_instruction(instruction, "turn off");
            execute_command(&mut lights, turn_off, &coordinates[0], &coordinates[1])
        } else if instruction.starts_with("toggle") {
            let coordinates = parse_instruction(instruction, "toggle");
            execute_command(&mut lights, toggle, &coordinates[0], &coordinates[1])
        }
    }

    println!("{:?}", lights.values().filter(|&state| *state).count());
}

fn parse_instruction(instruction: &str, command: &str) -> Vec<Vec<u32>> {
    let command_length = command.len() + 1;
    let ranges: Vec<&str> = instruction[command_length..].split(" through ").collect();
    ranges.iter().map(
        |range| range.split(",").map(
            |s| s.parse().unwrap()
            ).collect()
        ).collect()
}

fn execute_command(
    lights: &mut HashMap<(u32, u32), bool>,
    command: fn(&mut bool),
    start_range: &Vec<u32>,
    end_range: &Vec<u32>
    ) {
    for x in start_range[0]..=end_range[0] {
        for y in start_range[1]..=end_range[1] {
            lights.entry((x, y)).and_modify(command);
        }
    }
}

fn turn_on(state: &mut bool) {
    *state = true
}

fn turn_off(state: &mut bool) {
    *state = false
}

fn toggle(state: &mut bool) {
    *state = !*state
}
