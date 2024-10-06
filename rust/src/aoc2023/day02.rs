use std::cmp::max;
use std::io;

pub fn run() -> io::Result<()> {
    let file = include_str!("day02.txt");

    println!("Day 2 part 1");
    let lines = file.lines();
    let sum: u32 = lines
        .enumerate()
        .map(|(idx, l)| {
            if possible_game(rgb_max(l)) {
                idx as u32 + 1
            } else {
                0
            }
        })
        .sum();
    println!("Sum: {}", sum);

    println!("Day 2 part 2");
    let lines2 = file.lines();
    let sum2: u32 = lines2
        .map(|l| {
            let (r, g, b) = rgb_max(l);
            r * g * b
        })
        .sum();
    println!("Sum: {}", sum2);

    Ok(())
}

fn possible_game(rgb: RGB) -> bool {
    rgb.0 <= 12 && rgb.1 <= 13 && rgb.2 <= 14
}

fn rgb_max(game: &str) -> RGB {
    parse_game(game).iter().fold((0, 0, 0), |acc, &(r, g, b)| {
        (max(acc.0, r), max(acc.1, g), max(acc.2, b))
    })
}

fn parse_game(game: &str) -> Vec<RGB> {
    game.split(":")
        .nth(1)
        .unwrap()
        .trim()
        .split(";")
        .map(|r| extract_rgb(r))
        .collect()
}

type RGB = (u32, u32, u32);

fn extract_rgb(roll: &str) -> RGB {
    let mut rgb = (0, 0, 0);
    roll.split(",")
        .map(|x| x.trim().split_whitespace().collect::<Vec<_>>())
        .for_each(|parts| {
            let n = parts[0].parse::<u32>().unwrap();
            match parts[1] {
                "red" => rgb.0 += n,
                "green" => rgb.1 += n,
                "blue" => rgb.2 += n,
                _ => panic!("Unknown color {}", parts[1]),
            }
        });
    rgb
}

#[cfg(test)]
mod tests {
    use crate::aoc2023::day02::{extract_rgb, possible_game, rgb_max};

    #[test]
    fn test_rgb_max() {
        assert_eq!(
            rgb_max("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"),
            (4, 2, 6)
        );
        assert_eq!(
            rgb_max("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"),
            (1, 3, 4)
        );
        assert_eq!(
            rgb_max("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"),
            (20, 13, 6)
        );
        assert_eq!(
            rgb_max("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"),
            (14, 3, 15)
        );
        assert_eq!(
            rgb_max("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"),
            (6, 3, 2)
        );
    }

    #[test]
    fn test_possible_game() {
        assert!(possible_game((4, 2, 6)));
        assert!(possible_game((1, 3, 4)));
        assert!(!possible_game((20, 13, 6)));
        assert!(!possible_game((14, 3, 15)));
        assert!(possible_game((6, 3, 2)));
    }

    #[test]
    fn test_extract_rgb() {
        assert_eq!(extract_rgb("3 blue, 4 red"), (4, 0, 3));
        assert_eq!(extract_rgb(" 1 red, 2 green, 6 blue"), (1, 2, 6));
        assert_eq!(extract_rgb("2 green"), (0, 2, 0));
    }
}
