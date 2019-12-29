// https://adventofcode.com/2015/day/1

fn main() {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let instructions = input.trim_end();
    let mut floor = 0;
    let mut basement_position = 0;

    for (index, instruction) in instructions.chars().enumerate() {
        match instruction {
            '(' => { floor += 1 },
            ')' => { floor -= 1 },
            _ => println!("Ooops")
        }
        if basement_position == 0 && floor == -1 {
            basement_position = index + 1
        }
    }
    println!("{:?}", floor);
    println!("{:?}", basement_position);
}
