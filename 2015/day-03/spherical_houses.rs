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
    let mut santa_location = Location::new(0, 0);
    let mut robot_location = Location::new(0, 0);
    locations.insert(santa_location);

    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let directions = input.trim_end();

    for (turn, direction) in directions.chars().enumerate() {
        if turn % 2 == 0 {
            santa_location = match direction {
                '^' => santa_location.north(),
                'v' => santa_location.south(),
                '>' => santa_location.east(),
                '<' => santa_location.west(),
                _ => santa_location
            };
            locations.insert(santa_location);
        } else {
            robot_location = match direction {
                '^' => robot_location.north(),
                'v' => robot_location.south(),
                '>' => robot_location.east(),
                '<' => robot_location.west(),
                _ => robot_location
            };
            locations.insert(robot_location);
        }
    }

    println!("{:?}", locations.len());
}
