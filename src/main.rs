mod ast;

use std::env;
use crate::ast::lexer::Lexer;


const DEFAULT_EXTENSION: &str = ".malc";
const DEBUG: bool = false;
fn main() {

    let args: Vec<String> = env::args().collect();

    let input = "@name(\n\"AngleDraw\"\n)";

    let mut lexer = Lexer::new(input);
    while let Some(token) = lexer.next_token() {
        println!("{:#?}", token);
    }

    // @TODO: Add an evaluator
}

fn compile_file(file: &str) {

}