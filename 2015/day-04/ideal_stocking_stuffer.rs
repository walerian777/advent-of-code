// https://adventofcode.com/2015/day/4

use crypto::digest::Digest;
use crypto::md5::Md5;

fn main() {
    let secret_key = "ckczppom";
    let mut number = 1;
    let mut hasher = Md5::new();

    loop {
        let hash = format!("{}{}", secret_key, number.to_string());
        hasher.input_str(&hash);
        let digest = hasher.result_str();

        if digest.starts_with("00000") {
            println!("{:?}", number);
        }
        if digest.starts_with("000000") {
            println!("{:?}", number);
            break;
        }
        number += 1;
        hasher.reset();
    }
}
