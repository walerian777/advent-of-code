// https://adventofcode.com/2015/day/1

fn main() {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let instructions = input.trim_end();
    let mut floor = 0;

    for instruction in instructions.chars() {
        match instruction {
            '(' => { floor += 1 },
            ')' => { floor -= 1 },
            _ => println!("Ooops")
        }
    }
    println!("{:?}", floor);
}
