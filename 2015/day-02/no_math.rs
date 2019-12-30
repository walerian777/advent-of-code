// https://adventofcode.com/2015/day/2

struct PresentBox {
    length: u32,
    width: u32,
    height: u32
}

impl PresentBox {
    fn new(length: u32, width: u32, height: u32) -> PresentBox {
        PresentBox {
            length: length,
            width: width,
            height: height
        }
    }

    fn sides(&self) -> Vec<u32> {
        vec![
            self.length * self.width,
            self.length * self.height,
            self.width * self.height
        ]
    }

    fn smallest_side(&self) -> u32 {
        match self.sides().iter().min() {
            Some(value) => *value,
            None => 0
        }
    }

    fn surface(&self) -> u32 {
        let sides: u32 = self.sides().iter().sum();
        sides * 2
    }

    fn wrapping_paper(&self) -> u32 {
        self.surface() + self.smallest_side()
    }

    fn sizes(&self) -> Vec<u32> {
        vec![self.length, self.width, self.height]
    }

    fn smallest_perimeter(&self) -> u32 {
        let biggest_size: u32 = match self.sizes().iter().max() {
            Some(value) => *value,
            None => 0
        };
        let sides_sum: u32 = self.sizes().iter().sum();
        (sides_sum - biggest_size) * 2
    }

    fn volume(&self) -> u32 {
        self.length * self.width * self.height
    }
}

fn load_input() -> Vec<PresentBox> {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let sizes: Vec<Vec<u32>> = input.trim_end().split("\n").map(
        |line| line.split("x")
        .map(|size| size.parse().unwrap()).collect())
        .collect();
    sizes.iter()
        .map(|size| PresentBox::new(size[0], size[1], size[2]))
        .collect()
}

fn main() {
    let boxes = load_input();
    let wrapping_paper: u32 = boxes.iter().fold(0, |acc, b| acc + b.wrapping_paper());
    println!("{:?}", wrapping_paper);

    let ribbon: u32 = boxes.iter().fold(0, |acc, b| acc + b.volume() + b.smallest_perimeter());
    println!("{:?}", ribbon);
}
