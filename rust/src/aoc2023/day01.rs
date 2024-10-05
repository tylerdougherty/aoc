use std::collections::HashSet;
use std::io;

pub fn run() -> io::Result<()> {
    let file = include_str!("day01.txt");

    println!("Day 1 part 1");
    let lines = file.lines();
    let sum: u32 = lines.map(|l| get_calibration_value(l.to_string())).sum();
    println!("Sum: {}", sum);

    println!("Day 1 part 2");
    let lines2 = file.lines();
    let sum2: u32 = lines2.map(|l| get_calibration_value_2(l.to_string())).sum();
    println!("Sum: {}", sum2);

    Ok(())
}

fn maybe_digit(c: char) -> Option<u32> {
    let digits: HashSet<char> = "123456789".chars().into_iter().collect();
    if digits.contains(&c) { Some(c.to_string().parse().unwrap()) } else { None }
}

fn get_calibration_value(s: String) -> u32 {
    let mut l: Option<u32> = None;
    let mut r: Option<u32> = None;
    for c in s.chars() {
        if let Some(d) = maybe_digit(c) {
            if l == None { l = Some(d); }
            r = Some(d);
        }
    }

    format!("{}{}", l.unwrap(), r.unwrap()).parse::<u32>().unwrap()
}

fn get_calibration_value_2(s: String) -> u32 {
    let mut l: Option<u32> = None;
    let mut r: Option<u32> = None;
    for (i, c) in s.chars().enumerate() {
        if let Some(d) = maybe_digit(c) {
            if l == None { l = Some(d); }
            r = Some(d);
        }
        if let Some(d) = maybe_digit_str(&s[i..]) {
            if l == None { l = Some(d); }
            r = Some(d);
        }
    }

    format!("{}{}", l.unwrap(), r.unwrap()).parse::<u32>().unwrap()
}

fn maybe_digit_str(s: &str) -> Option<u32> {
    let nums = vec!["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

    for (i, num) in nums.iter().enumerate() {
        if s.starts_with(num) { return Some((i + 1) as u32); }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::aoc2023::day01::{get_calibration_value, maybe_digit, maybe_digit_str};

    #[test]
    fn test_calibration_values() {
        assert_eq!(get_calibration_value(String::from("1a2b3c4")), 14);
        assert_eq!(get_calibration_value(String::from("1abc2")), 12);
        assert_eq!(get_calibration_value(String::from("pqr3stu8vwx")), 38);
        assert_eq!(get_calibration_value(String::from("a1b2c3d4e5f")), 15);
        assert_eq!(get_calibration_value(String::from("treb7uchet")), 77);
    }

    #[test]
    fn test_maybe_digit() {
        assert_eq!(maybe_digit('0'), None);
        assert_eq!(maybe_digit('7'), Some(7));
        assert_eq!(maybe_digit('9'), Some(9));
        assert_eq!(maybe_digit('a'), None);
    }

    #[test]
    fn test_maybe_digit_str() {
        assert_eq!(maybe_digit_str("0"), None);
        assert_eq!(maybe_digit_str("onetwothree"), Some(1));
        assert_eq!(maybe_digit_str("sixteen"), Some(6));
    }
}