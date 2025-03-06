use blazepdf_lib::blaze_generate;
use clap::Parser;
use std::fs;

const BLAZEPDF_INTRO: &str = r#"
        ____  __                 ____  ____  ______
       / __ )/ /___ _____  ___  / __ \/ __ \/ ____/
      / __  / / __ `/_  / / _ \/ /_/ / / / / /_    
     / /_/ / / /_/ / / /_/  __/ ____/ /_/ / __/    
    /_____/_/\__,_/ /___/\___/_/   /_____/_/    

    Welcome to BlazePDF - The Rust-Powered HTML-to-PDF Engine!
"#;

#[derive(Parser)]
#[command(name = "BlazePDF")]
#[command(about = "Convert HTML to PDF using Rust")]
struct Args {
    /// Input file name.
    // #[arg(short, long)]
    input: String,

    /// Output file name.
    // #[arg(short, long)]
    output: String,
}

fn main() {
    println!("{}", BLAZEPDF_INTRO);

    // parse the args given in terminal
    let args: Args = Args::parse();

    match fs::read_to_string(&args.input) {
        Ok(html_content) => {
            println!("Successfully read the HTML file.");
            blaze_generate::blaze_pdf::generate(&html_content, "");
        }
        Err(e) => {
            eprintln!("Error reading HTML file: {}", e);
            std::process::exit(1);
        }
    }
}
