#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    IntNumber(i64),
    FloatNumber(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    ArrayCall,
    // keywords
    Fn,
    Struct,
    Point,
    Warpto,
    WarptoIf,
    Return,
    Import,

    // signs & operators
    Comma,
    Semicolon,
    Colon,
    Lsep(LSeparator),
    Rsep(RSeparator),

    EOF,
}
#[derive(Debug, Clone, PartialEq)]
pub enum LSeparator {
    LParen,
    LBrace,
}
#[derive(Debug, Clone, PartialEq)]
pub enum RSeparator {
    RParen,
    RBrace,
}
pub struct Tokenizer {
    chars: Vec<char>,
    pos: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).cloned()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        if self.peek() == Some('/') {
            let next = self.chars.get(self.pos + 1);
            if next == Some(&'/') {
                self.pos += 2;
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        self.pos += 1;
                        self.skip_comment();
                        self.skip_whitespace();
                        break;
                    }
                    self.pos += 1;
                }
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();
            self.skip_comment();
            break;
        }

        let ch = match self.next_char() {
            Some(c) => c,
            None => return Token::EOF,
        };

        // ----- String -----
        if ch == '"' {
            let mut s = String::new();
            while let Some(c) = self.next_char() {
                if c == '"' {
                    break;
                }
                if c != '\\' {
                    s.push(c);
                } else {
                    if self.match_next('n') {
                        s.push('\n');
                    } else if self.match_next('0') {
                        s.push('\0');
                    } else if self.match_next('\\') {
                        s.push('\\');
                    } else if self.match_next('r') {
                        s.push('\r');
                    } else if self.match_next('t') {
                        s.push('\t');
                    } else if self.match_next('"') {
                        s.push('"');
                    } else if self.match_next('x') {
                        let h1 = self.next_char().unwrap();
                        let h2 = self.next_char().unwrap();
                        let byte = u8::from_str_radix(&format!("{}{}", h1, h2), 16).unwrap();
                        s.push(byte as char);
                    } else if self.match_next('u') {
                        if self.match_next('{') {
                            let mut hex = String::new();

                            while let Some(cc) = self.next_char() {
                                if cc == '}' {
                                    break;
                                }
                                hex.push(cc);
                            }
                            let cp = u32::from_str_radix(&hex, 16).unwrap();
                            let cch = char::from_u32(cp).unwrap();
                            s.push(cch);
                        } else {
                            s.push('\\');
                            s.push('u');
                        }
                    } else {
                        s.push(c);
                    }
                }
            }

            return Token::StringLiteral(s);
        }
        if ch == '\'' {
            let mut s = String::new();
            while let Some(c) = self.next_char() {
                if c == '\'' {
                    break;
                }
                if c != '\\' {
                    s.push(c);
                } else {
                    if self.match_next('n') {
                        s.push('\n');
                    } else if self.match_next('0') {
                        s.push('\0');
                    } else if self.match_next('\\') {
                        s.push('\\');
                    } else if self.match_next('r') {
                        s.push('\r');
                    } else if self.match_next('t') {
                        s.push('\t');
                    } else if self.match_next('"') {
                        s.push('"');
                    } else if self.match_next('x') {
                        let h1 = self.next_char().unwrap();
                        let h2 = self.next_char().unwrap();
                        let byte = u8::from_str_radix(&format!("{}{}", h1, h2), 16).unwrap();
                        s.push(byte as char);
                    } else if self.match_next('u') {
                        if self.match_next('{') {
                            let mut hex = String::new();

                            while let Some(cc) = self.next_char() {
                                if cc == '}' {
                                    break;
                                }
                                hex.push(cc);
                            }
                            let cp = u32::from_str_radix(&hex, 16).unwrap();
                            let cch = char::from_u32(cp).unwrap();
                            s.push(cch);
                        } else {
                            s.push('\\');
                            s.push('u');
                        }
                    } else {
                        s.push(c);
                    }
                }
            }
            let c = s.chars().collect::<Vec<_>>()[0];
            return Token::CharLiteral(c);
        }

        // ----- Number -----
        if ch.is_ascii_digit() || ch == '-' {
            let mut s = ch.to_string();
            let mut is_float = false;
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() || c == '.' {
                    if c == '.' {
                        is_float = true
                    }
                    s.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if s != "-" {
                if is_float {
                    return Token::FloatNumber(s.parse::<f64>().unwrap());
                } else {
                    return Token::IntNumber(s.parse::<i64>().unwrap());
                }
            }
        }

        // ----- Identifier / Keyword -----
        if !ch.is_ascii_digit()
            && ch != ','
            && ch != ';'
            && ch != '{'
            && ch != '}'
            && ch != '('
            && ch != ')'
            && ch != ':'
        {
            let mut s = ch.to_string();
            while let Some(c) = self.peek() {
                if c != ','
                    && c != ';'
                    && c != '{'
                    && c != '}'
                    && c != '('
                    && c != ')'
                    && c != ' '
                    && c != ':'
                    && c != '\n'
                {
                    s.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }

            return match s.as_str() {
                "fn" => Token::Fn,
                "struct" => Token::Struct,
                "point" => Token::Point,
                "warpto" => Token::Warpto,
                "warptoif" => Token::WarptoIf,
                "true" => Token::BoolLiteral(true),
                "false" => Token::BoolLiteral(false),
                "return" => Token::Return,
                "Array" => Token::ArrayCall,
                "import" => Token::Import,
                _ => Token::Ident(s),
            };
        }

        // ----- Operators / Signs -----
        match ch {
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::Lsep(LSeparator::LParen),
            ')' => Token::Rsep(RSeparator::RParen),
            '{' => Token::Lsep(LSeparator::LBrace),
            '}' => Token::Rsep(RSeparator::RBrace),
            _ => Token::EOF,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let t = self.next_token();
            if t == Token::EOF {
                break;
            }
            tokens.push(t);
        }

        tokens
    }
}
