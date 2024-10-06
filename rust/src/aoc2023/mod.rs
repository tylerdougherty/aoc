use std::io;

mod day01;
mod day02;

pub fn run_all() -> io::Result<()> {
    print_line_separator();
    day01::run()?;
    print_line_separator();
    day02::run()?;

    Ok(())
}

fn print_line_separator() {
    println!("------------------------------")
}