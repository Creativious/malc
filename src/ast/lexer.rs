

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Integer(i64),
    Float(f64),
    Add,
    Sub,
    Exp,
    Mul,
    Div,
    LParen,
    RParen,
    EOF,
    Bad,
    Whitespace,
    String(String),
    Identifier(String), // @TODO: Make this a Identifier struct, takes a String and a bool saying if it's global or not (yuck globals), globals must be defined with const
    Definition, // let val, other_val = 1 || const val = 5 || val = 3 @TODO: Make this a Definition struct, contains a Vec<Identifier> and a Expression, but ofc both are tokenized
    // Definitions are both initializer and the updater
    // Definition structs can also take a bool saying if it's a global or not along with a bool saying if it's on initialization or not
    Comparative, // '==', '!=', '>', '<', '>=', '<=' @TODO: Make this a Comparative struct, takes a ComparativeKind
    While, // @TODO:: Make this a While struct
    Loop, // @TODO:: Make this a Loop struct
    Expression(Expression),
    HardCall(HardCall),
    Override(HardCall),
    NewLine,
    FunctionCall(String), // @TODO: Make this take a FunctionCall struct
    Comma,
    FunctionDefinition(FunctionDefinition),
    Semicolon,
    None, // all functions return this if nothing is specified

    // Calling App.menu.was_pressed(0) will return these tokens in order, StructCall("App"), StructReference("menu"), FunctionCall("was_pressed"), Integer(0)
    StructCall, // @TODO: Make this take a StructCall struct
    StructReference, // @TODO: Make this take a StructReference struct,
    StructDefinition, // @TODO: Maybe some time in the future I'll make it so you can make custom structs, though imma have to be janky on how it converts to the hp prime programming language
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    name: String,
    tokens: Vec<Token>,
    args: Vec<Token>,
    return_type: Box<Token>,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextSpan,
}

#[derive(Debug, PartialEq)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) literal: String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal
        }
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

impl Token {

    pub fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self {
            kind,
            span
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    current_position: usize,
}

#[derive(Debug, PartialEq)]
pub struct HardCall {
    name: String,
    args: Vec<Token>,
}

impl HardCall {
    pub fn new(name: String, args: Vec<Token>) -> Self {
        Self {
            name,
            args
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Argument,
    Definition,
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    tokens: Vec<Token>,
    kind: ExpressionKind,
}

pub struct Number {
    int: i64,
    float: f64,
}

impl Number {
    pub fn new_int(int: i64) -> Self {
        Self {
            int,
            float: 0.0
        }
    }
    pub fn new_float(float: f64) -> Self {
        Self {
            int: 0,
            float
        }
    }
    pub fn is_int(&self) -> bool {
        self.int != 0
    }
    pub fn is_float(&self) -> bool {
        self.float != 0.0
    }

    pub fn flip_sign(&mut self) {
        if self.is_int() {
            self.int *= -1;
        } else if self.is_float() {
            self.float *= -1.0;
        }
    }
}

impl Expression {
    pub fn new(tokens: Vec<Token>, kind: ExpressionKind) -> Self {
        Self {
            tokens,
            kind,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            current_position: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.current_position > self.input.len() {
            return None;
        }
        if self.current_position == self.input.len() {
            self.current_position += 1;
            return Some(self.get_eof_token());
        }
        let token = self.parse_token();
        Some(token)
    }

    fn parse_token(&mut self) -> Token {
        let start = self.current_position;
        if self.current_char().is_none() {
            self.consume_char();
            let end = self.current_position;
            return Token::new(TokenKind::Bad, TextSpan::new(start, end, self.input[start..end].to_string()));
        }
        let c = self.current_char().unwrap();
        if Self::is_hard_call(&c) {
            let option_hard_call = self.consume_hard_call();
            let end = self.current_position;
            if let Some(hard_call) = option_hard_call {
                if hard_call.name == "override" {
                    return Token::new(TokenKind::Override(hard_call), TextSpan::new(start, end, self.input[start..end].to_string()));
                }
                return Token::new(TokenKind::HardCall(hard_call), TextSpan::new(start, end, self.input[start..end].to_string()));
            }
        } else if Self::is_string_start(&c) {
            let option_string = self.consume_string();
            let end = self.current_position;
            if let Some(string) = option_string {
                return Token::new(TokenKind::String(string), TextSpan::new(start, end, self.input[start..end].to_string()));
            }
        } else if Self::is_operator(&c) {
            let operator = self.consume_operator();
            if operator == TokenKind::Sub {
                let next_c = self.current_char();
                if next_c.is_some() && Self::is_number_start(&next_c.unwrap()) {
                    let mut number = self.consume_number();
                    let end = self.current_position;
                    if let Some(mut number) = number {
                        number.flip_sign();
                        return self.tokenize_number(number, start, end);
                    }
                }
            }
            if operator == TokenKind::Mul {
                let next_c = self.current_char();
                if next_c.is_some() && next_c.unwrap() == '*' {
                    self.consume_char();
                    let end = self.current_position;
                    return Token::new(TokenKind::Exp, TextSpan::new(start, end, self.input[start..end].to_string()));
                }
            }
            let end = self.current_position;
            return Token::new(operator, TextSpan::new(start, end, self.input[start..end].to_string()));

        } else if Self::is_number_start(&c) {
            let number = self.consume_number();
            let end = self.current_position;
            if let Some(number) = number {
                return self.tokenize_number(number, start, end);
            }
        } else if Self::is_new_line(&c) {
            self.consume_char();
            let end = self.current_position;
            return Token::new(TokenKind::NewLine, TextSpan::new(start, end, self.input[start..end].to_string()));
        } else if Self::is_whitespace(&c) {
            self.consume_whitespace();
            let end = self.current_position;
            return Token::new(TokenKind::Whitespace, TextSpan::new(start, end, self.input[start..end].to_string()));
        }
        // @TODO: add token handling for functions, variables, bools, function calls, class calls
        self.consume_char();
        let end = self.current_position;
        Token::new(TokenKind::Bad, TextSpan::new(start, end, self.input[start..end].to_string()))
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if !Self::is_whitespace(&c) {
                break;
            }
            self.consume_char();
        }
    }

    fn is_new_line(c: &char) -> bool {
        c == &'\n'
    }

    fn tokenize_number(&mut self, number: Number, start: usize, end: usize) -> Token {
        if number.is_int() {
            Token::new(TokenKind::Integer(number.int), TextSpan::new(start, end, self.input[start..end].to_string()))
        }
        else if number.is_float() {
            Token::new(TokenKind::Float(number.float), TextSpan::new(start, end, self.input[start..end].to_string()))
        }
        else {
            Token::new(TokenKind::Bad, TextSpan::new(start, end, self.input[start..end].to_string()))
        }
    }

    fn consume_number(&mut self) -> Option<Number> {
        let mut val: i64 = 0;
        let mut decimal_point_position = self.current_position;
        let mut decimal_point = false;
        while let Some(c) = self.current_char() {
            if c.is_digit(10) {
                self.consume_char();
                val = val * 10 + c.to_digit(10).unwrap() as i64;
            }
            else if c == '.' && !decimal_point {
                decimal_point = true;
                decimal_point_position = self.current_position;
                self.consume_char();
            }
            else if c == '.' && decimal_point {
                self.consume_char();
                return None;
            }
            else {
                break;
            }
        }
        if (decimal_point && decimal_point_position == self.current_position) {
            return None;
        }
        if decimal_point {
            Some(Number::new_float(val as f64 / 10.0_f64.powi((self.current_position - decimal_point_position - 1) as i32)))
        }
        else {
            Some(Number::new_int(val))
        }

    }

    fn is_number_start(c: &char) -> bool {
        c.is_digit(10)
    }

    fn is_operator(c: &char) -> bool {
        c.eq(&'+') || c.eq(&'-') || c.eq(&'*') || c.eq(&'/') || c.eq(&'(') || c.eq(&')')
    }

    fn consume_operator(&mut self) -> TokenKind {
        let c = self.current_char().unwrap();
        let kind = match c {
            '+' => TokenKind::Add,
            '-' => TokenKind::Sub,
            '*' => TokenKind::Mul,
            '/' => TokenKind::Div,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            _ => TokenKind::Bad
        };
        self.consume_char();
        kind
    }

    fn consume_arg(&mut self) -> Token {
        let start = self.current_position;
        // an arg is always an expression
        let expression = self.consume_expression(ExpressionKind::Argument);
        let end = self.current_position;
        if expression.is_none() {
            return Token::new(TokenKind::Bad, TextSpan::new(start, end, self.input[start..end].to_string()));
        }
        Token::new(TokenKind::Expression(expression.unwrap()), TextSpan::new(start, end, self.input[start..end].to_string()))
    }

    fn consume_expression(&mut self, kind: ExpressionKind) -> Option<Expression> {
        // let val = 1 + 5 + 7; // the stuff after the '=' is an expression
        // function(a + 5, b, c) // a + 5, b, c are all expressions

        let mut tokens: Vec<Token> = Vec::new();
        while let Some(c) = self.current_char() {
            if kind == ExpressionKind::Argument && (c == ')' || c == ',') {
                break;
            }
            else if kind == ExpressionKind::Definition && c == ';' {
                break;
            }
            tokens.push(self.parse_token());
        }

        if tokens.is_empty() {
            return None;
        }
        Some(Expression::new(tokens, kind))
    }

    fn next_char(&mut self) -> Option<char> {
        Some(self.input.chars().nth(self.current_position + 1).unwrap())
    }

    fn is_hard_call(c: &char) -> bool {
        c.eq(&'@')
    }

    fn is_string_start(c: &char) -> bool {
        c.eq(&'"')
    }

    fn consume_hard_call(&mut self) -> Option<HardCall> {
        let mut name = String::new();
        let mut args: Vec<Token> = Vec::new();
        self.consume_char(); // skip past the @ sign

        // get the name
        while let Some(c) = self.current_char() {
            self.consume_char();
            if c == '(' {
                break;
            }
            if !c.is_alphabetic() && !(c == '_') {
                return None;
            }
            name.push(c);
        }

        // get the args
        while let Some(c) = self.current_char() {
            if c == ')' {
                self.consume_char();
                break;
            }
            args.push(self.consume_arg());
        }

        // check for bad args
        for arg in args.iter() {
            if arg.kind == TokenKind::Bad {
                return None;
            }
        }
        Some(HardCall::new(name, args))
    }

    fn consume_string(&mut self) -> Option<String> {
        let mut string = String::new();
        self.consume_char(); // skips past the first '"'
        while let Some(c) = self.current_char() {
            if c == '\n' && !self.previous_char()?.eq(&'\\') {
                return None;
            }
            if c == '\\' {
                self.consume_char();
                continue;
            }
            self.consume_char();
            if c == '"' {
                break;
            }
            string.push(c);
        }
        Some(string)
    }

    fn previous_char(&mut self) -> Option<char> {
        Some(self.input.chars().nth(self.current_position - 1).unwrap())
    }

    fn peek_char(&mut self, offset: isize) -> Option<char> {
        Some(self.input.chars().nth((self.current_position as isize + offset) as usize).unwrap())
    }

    fn consume_char(&mut self) -> Option<char> {
        self.current_position += 1;
        Some(self.input.chars().nth(self.current_position - 1).unwrap())
    }

    fn current_char(&mut self) -> Option<char> {
        self.input.chars().nth(self.current_position)
    }

    fn get_eof_token(&self) -> Token {
        Token::new(TokenKind::EOF, TextSpan::new(self.current_position, self.current_position, String::from("\0")))
    }

    fn get_blank_bad_token(&self) -> Token {
        Token::new(TokenKind::Bad, TextSpan::new(self.current_position, self.current_position, String::from("\0")))
    }
}