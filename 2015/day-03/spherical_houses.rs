// https://adventofcode.com/2015/day/3

use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Location {
    latitude: i32,
    longitude: i32
}

impl Location {
    fn new(latitude: i32, longitude: i32) -> Location {
        Location { latitude: latitude, longitude: longitude }
    }

    fn north(&self) -> Location {
        Location::new(self.latitude + 1, self.longitude)
    }

    fn south(&self) -> Location {
        Location::new(self.latitude - 1, self.longitude)
    }

    fn east(&self) -> Location {
        Location::new(self.latitude, self.longitude + 1)
    }

    fn west(&self) -> Location {
        Location::new(self.latitude, self.longitude - 1)
    }
}

fn main() {
    let mut locations = HashSet::new();
    let mut current_location = Location::new(0, 0);
    locations.insert(current_location);

    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let directions = input.trim_end();

    for direction in directions.chars() {
        current_location = match direction {
            '^' => current_location.north(),
            'v' => current_location.south(),
            '>' => current_location.east(),
            '<' => current_location.west(),
            _ => current_location
        };
        locations.insert(current_location);
    }

    println!("{:?}", locations.len());
}
