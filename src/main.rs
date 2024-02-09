mod lexer;
mod parser;
mod token;
mod ty;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Please provide source");
        std::process::exit(1);
    }

    let file_name = args.iter().nth(1).unwrap();
    let path = std::path::Path::new(&file_name);

    if !path.exists() {
        eprintln!("no such file {}", path.display());
        std::process::exit(1);
    }

    let contents = std::fs::read_to_string(path).expect("Failed to read file");
    println!("LEN: {}", contents.len());
    let toks = lexer::tokenize(&contents);

    let parser = parser::Parser::new(&contents, &toks);
    parser.parse();

    // for tok in toks {
    //     println!("{tok:?},");
    // }
}
