// https://adventofcode.com/2015/day/5

extern crate regex;
use regex::Regex;

fn is_nice_string(string: &str) -> bool {
    has_at_least_three_vowels(string) && has_doubled_letter(string) && !contains_disallowed_substring(string)
}

fn has_at_least_three_vowels(string: &str) -> bool {
    let re = Regex::new(r"(.*(a|e|i|o|u)){3}").unwrap();
    re.is_match(string)
}

fn has_doubled_letter(string: &str) -> bool {
    let re = Regex::new(r"(aa|bb|cc|dd|ee|ff|gg|hh|ii|jj|kk|ll|mm|nn|oo|pp|qq|rr|ss|tt|uu|vv|ww|xx|yy|zz)").unwrap();
    re.is_match(string)
}

fn contains_disallowed_substring(string: &str) -> bool {
    let re = Regex::new(r"(ab|cd|pq|xy)").unwrap();
    re.is_match(string)
}

fn main() {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let strings: Vec<&str> = input.trim_end().split("\n").collect();
    let nice_words: u32 = strings.iter().fold(0, |acc, s| acc + is_nice_string(s) as u32);
    println!("{}", nice_words);
}
