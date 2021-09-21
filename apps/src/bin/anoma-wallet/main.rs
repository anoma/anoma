mod cli;
use color_eyre::eyre::Result;

pub fn main() -> Result<()> {
    color_eyre::install()?;

    // run the CLI
    cli::main()
}
