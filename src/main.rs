mod codegen;
mod debug;
mod dump_ast;
mod lexer;
mod node;
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

    let mut codegen_out: Box<dyn std::io::Write> = if let Some(out) = args.iter().nth(2) {
        if out != "-o" {
            eprintln!("Unknown param {out}");
            std::process::exit(1);
        }
        let Some(out_file) = args.iter().nth(3) else {
            eprintln!("No output file name specified with -o");
            std::process::exit(1);
        };
        let file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&out_file);
        if let Err(e) = file {
            eprintln!("Failed to open file, error: {e}");
            std::process::exit(1);
        }
        Box::new(file.unwrap())
    } else {
        Box::new(std::io::stdout())
    };

    if !path.exists() {
        eprintln!("no such file {}", path.display());
        std::process::exit(1);
    }

    let contents = std::fs::read_to_string(path).expect("Failed to read file");
    let toks = lexer::tokenize(&contents);

    let mut parser = parser::Parser::new(&contents, &toks);
    parser.parse();

    // for g in &parser.globals {
    //     println!("{g:?}");
    // }

    // crate::dump_ast::dump_ast(&parser.globals);
    // println!("\n----\n");

    codegen::generate(codegen_out.as_mut(), &parser.globals);
}
