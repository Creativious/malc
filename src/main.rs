mod ast;

use std::env;
use std::fs::File;
use std::io::{Read, Write};
use crate::ast::lexer::Lexer;


const DEFAULT_EXTENSION: &str = ".malc";
const DEBUG: bool = false;
fn main() {

    let args: Vec<String> = env::args().collect();

    // Make an input that takes test.txt reads it and outputs it as one big string
    let mut input = String::new();
    let mut file = File::open("test.txt").expect("File not found");
    file.read_to_string(&mut input).expect("Error reading file");

    let mut lexer = Lexer::new(&*input);
    let mut output = String::new();
    while let Some(token) = lexer.next_token() {
        // println!("{:#?}", token);
        let mut output_line = format!("{:#?}", token).as_str().to_owned() + "\n";
        output.push_str((output_line).as_str());
    }
    // Make the output string look better

    let mut file = File::create("output.txt").expect("Error creating file");
    file.write_all(output.as_bytes()).expect("Error writing to file");

    // @TODO: Add an evaluator, which evaluate the tokens, also handles errors
}

fn compile_file(file: &str) {

}