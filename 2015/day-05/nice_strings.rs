// https://adventofcode.com/2015/day/5

extern crate regex;
use regex::Regex;
use std::process::Command;

fn is_nice_string(string: &str) -> bool {
    has_at_least_three_vowels(string) && has_doubled_letter(string) && !contains_disallowed_substring(string)
}

fn has_at_least_three_vowels(string: &str) -> bool {
    let re = Regex::new(r"(.*(a|e|i|o|u)){3}").unwrap();
    re.is_match(string)
}

fn has_doubled_letter(string: &str) -> bool {
    let re = Regex::new(
        r"(aa|bb|cc|dd|ee|ff|gg|hh|ii|jj|kk|ll|mm|nn|oo|pp|qq|rr|ss|tt|uu|vv|ww|xx|yy|zz)"
        ).unwrap();
    re.is_match(string)
}

fn contains_disallowed_substring(string: &str) -> bool {
    let re = Regex::new(r"(ab|cd|pq|xy)").unwrap();
    re.is_match(string)
}

fn is_really_nice_string(string: &str) -> bool {
    has_doubled_pair(string) && has_pair_with_middleman(string)
}

fn has_doubled_pair(string: &str) -> bool {
    let grep_command = format!("echo {} | grep '\\(..\\).*\\1'", string);
    let grep_output = Command::new("sh").arg("-c")
        .arg(grep_command)
        .output().unwrap();
    let grep_result = String::from_utf8_lossy(&grep_output.stdout);
    grep_result.len() > 0
}

fn has_pair_with_middleman(string: &str) -> bool {
    let re = Regex::new(
        r"(a.a|b.b|c.c|d.d|e.e|f.f|g.g|h.h|i.i|j.j|k.k|l.l|m.m|n.n|o.o|p.p|q.q|r.r|s.s|t.t|u.u|v.v|w.w|x.x|y.y|z.z)"
        ).unwrap();
    re.is_match(string)
}

fn main() {
    let input = std::fs::read_to_string("input")
        .expect("Oops");
    let strings: Vec<&str> = input.trim_end().split("\n").collect();
    let nice_words: u32 = strings.iter().fold(0, |acc, s| acc + is_nice_string(s) as u32);
    println!("{}", nice_words);

    let really_nice_words: u32 = strings.iter().fold(0, |acc, s| acc + is_really_nice_string(s) as u32);
    println!("{}", really_nice_words);
}
