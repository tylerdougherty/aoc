use std::cmp::{max, min};
use std::collections::HashSet;
use std::io;
use std::iter::{IntoIterator, Iterator};

pub fn run() -> io::Result<()> {
    let file = include_str!("day03.txt").trim();

    println!("Day 3 part 1");
    let schematic = as_schematic(file);
    let sum: u32 = extract_part_numbers(&schematic)
        .iter()
        .filter(|n| n.touching_symbol(&schematic))
        .map(|n| n.get(&schematic))
        .sum();
    println!("Sum: {}", sum);

    Ok(())
}

type Schematic = Vec<Vec<char>>;

fn as_schematic(s: &str) -> Schematic {
    s.lines().map(|l| l.chars().collect()).collect()
}

#[derive(Debug)]
struct PartNumber {
    row: usize,
    col: usize,
    length: usize,
}

impl PartNumber {
    fn new(row: usize, col: usize, length: usize) -> Self {
        PartNumber { row, col, length }
    }

    fn get(&self, schematic: &Schematic) -> u32 {
        schematic[self.row][self.col..self.col + self.length]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    fn touching_symbol(&self, schematic: &Schematic) -> bool {
        // we know the digits of the number itself aren't symbols
        let non_symbols: HashSet<char> = "0123456789.".chars().into_iter().collect();

        let r_min = max(1, self.row) - 1;
        let r_max = min(schematic.len() - 1, self.row + 1);
        let c_min = max(1, self.col) - 1;
        let c_max = min(schematic.len() - 1, self.col + self.length);
        // println!("rows {}-{} cols {}-{}, both inclusive", r_min, r_max, c_min, c_max);

        for r in r_min..=r_max {
            for c in c_min..=c_max {
                if !non_symbols.contains(&schematic[r][c]) {
                    return true;
                }
            }
        }

        false
    }
}

fn extract_part_numbers(s: &Schematic) -> Vec<PartNumber> {
    let nums: HashSet<char> = "0123456789".chars().into_iter().collect();
    let mut result: Vec<PartNumber> = Vec::new();

    for (idx, row) in s.iter().enumerate() {
        let mut col = 0;
        while col < row.len() {
            if nums.contains(&row[col]) {
                let c = col;
                while col < row.len() && nums.contains(&row[col]) {
                    col += 1
                }
                result.push(PartNumber::new(idx, c, col - c));
            }

            col += 1
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::aoc2023::day03::{as_schematic, extract_part_numbers};

    #[test]
    fn extraction() {
        let schematic = as_schematic(
            "467..114..\n\
                ...*......\n\
                ..35..633.\n\
                .....&#...\n\
                617*......\n\
                .....+.58.\n\
                ..592.....\n\
                ......755.\n\
                ...$.*....\n\
                .664.598..",
        );
        let nums = extract_part_numbers(&schematic);
        nums.iter().for_each(|n| println!("{:?}", n));
        assert_eq!(nums.len(), 10);
        assert_eq!(nums[0].get(&schematic), 467);
        assert_eq!(nums[9].get(&schematic), 598);

        assert!(nums[0].touching_symbol(&schematic));
        assert!(!nums[1].touching_symbol(&schematic));
        assert!(nums[9].touching_symbol(&schematic));

        let sum: u32 = nums
            .iter()
            .filter(|n| n.touching_symbol(&schematic))
            .map(|n| n.get(&schematic))
            .sum();
        assert_eq!(sum, 4361);
    }
}
