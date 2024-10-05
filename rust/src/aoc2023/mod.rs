use std::io;

mod day01;

pub fn run_all() -> io::Result<()> {
    print_line_separator();
    day01::run()?;

    Ok(())
}

fn print_line_separator() {
    println!("------------------------------")
}