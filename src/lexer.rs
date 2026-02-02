/// Tokens produced by the Tokenizer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Identifier (variable name, function name, etc.)
    Ident(String),
    /// isize integer literal (e.g., 10_)
    IsizeNumber(isize),
    /// i64 integer literal (e.g., 10)
    IntNumber(i64),
    /// f64 float literal (e.g., 10.5)
    FloatNumber(f64),
    /// i32 integer literal (e.g., 10s)
    IntshortNumber(i32),
    /// f32 float literal (e.g., 10.5s)
    FloatshortNumber(f32),
    /// String literal (e.g., "hello")
    StringLiteral(String),
    /// Character literal (e.g., 'a')
    CharLiteral(char),
    /// Boolean literal
    BoolLiteral(bool),
    /// Array constructor call
    ArrayCall,

    // keywords
    Fn,
    Struct,
    Point,
    Warpto,
    WarptoIf,
    Return,
    Import,
    LoopIf,
    Declare,
    If,
    ElIf,
    Else,
    Export,

    // signs & operators
    Comma,
    Semicolon,
    Colon,
    /// Left separators ( ( or { )
    Lsep(LSeparator),
    /// Right separators ( ) or } )
    Rsep(RSeparator),

    /// End of file
    Eof,
}
/// Left-side separator tokens.
#[derive(Debug, Clone, PartialEq)]
pub enum LSeparator {
    /// (
    LParen,
    /// {
    LBrace,
}

/// Right-side separator tokens.
#[derive(Debug, Clone, PartialEq)]
pub enum RSeparator {
    /// )
    RParen,
    /// }
    RBrace,
}

/// Tokenizer for WapL source code.
pub struct Tokenizer {
    chars: Vec<char>,
    pos: usize,
}

impl Tokenizer {
    /// Creates a new Tokenizer from the given input string.
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    /// Peeks specifically at the current character.
    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).cloned()
    }

    /// Advances the position and returns the current character.
    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    /// Checks if the next character matches the expected character and advances if it does.
    fn match_next(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    /// Skips all whitespace and comments.
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.pos += 1;
            } else {
                self.skip_comment();
                break;
            }
        }
    }

    /// Skips single-line comments starting with `//`.
    fn skip_comment(&mut self) {
        loop {
            if self.peek() != Some('/') || self.chars.get(self.pos + 1) != Some(&'/') {
                return;
            }

            self.pos += 2;

            while let Some(c) = self.peek() {
                self.pos += 1;
                if c == '\n' {
                    break;
                }
            }

            self.skip_whitespace();
        }
    }

    /// Internal helper to tokenize strings and characters, handling escape sequences.
    fn string_and_char_tokenize(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.next_char() {
            if c == '"' || c == '\'' {
                break;
            }
            if c != '\\' {
                s.push(c);
                continue;
            }

            // Handle escape sequences
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
            } else if self.match_next('\'') {
                s.push('\'');
            } else if self.match_next('x') {
                // Hex escape (e.g., \x41)
                let h1 = self.next_char().unwrap();
                let h2 = self.next_char().unwrap();
                let byte = u8::from_str_radix(&format!("{}{}", h1, h2), 16).unwrap();
                s.push(byte as char);
            } else if self.match_next('u') {
                // Unicode escape (e.g., \u{1234})
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
        s
    }

    /// Returns the next token in the input stream.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comment();

        let ch = match self.next_char() {
            Some(c) => c,
            None => return Token::Eof,
        };

        // ----- String literal -----
        if ch == '"' {
            return Token::StringLiteral(self.string_and_char_tokenize());
        }
        // ----- Char literal -----
        if ch == '\'' {
            let c = self.string_and_char_tokenize().chars().collect::<Vec<_>>()[0];
            return Token::CharLiteral(c);
        }

        // ----- Number literal -----
        if ch.is_ascii_digit() || ch == '-' {
            let mut s = ch.to_string();
            let mut is_float = false;
            let mut is_short = false;
            let mut is_isize = false;
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() || c == '.' || c == 's' || c == '_' {
                    if c == '.' {
                        is_float = true;
                    }
                    if c == '_' {
                        // isize indicator (e.g., 10_)
                        is_short = false;
                        is_float = false;
                        is_isize = true;
                        self.pos += 1;
                        break;
                    }
                    if c == 's' {
                        // short indicator (e.g., 10s -> i32/f32)
                        is_short = true;
                        self.pos += 1;
                        break;
                    }
                    s.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if s != "-" {
                if is_isize {
                    return Token::IsizeNumber(s.parse::<isize>().unwrap());
                }
                if is_float {
                    return if !is_short {
                        Token::FloatNumber(s.parse::<f64>().unwrap())
                    } else {
                        Token::FloatshortNumber(s.parse::<f32>().unwrap())
                    };
                } else {
                    return if !is_short {
                        Token::IntNumber(s.parse::<i64>().unwrap())
                    } else {
                        Token::IntshortNumber(s.parse::<i32>().unwrap())
                    };
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
                "import" | "use" => Token::Import,
                "loopif" => Token::LoopIf,
                "declare" => Token::Declare,
                "if" => Token::If,
                "elif" => Token::ElIf,
                "else" => Token::Else,
                "export" => Token::Export,
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
            _ => Token::Eof,
        }
    }

    /// Tokenizes the entire input string and returns a vector of tokens.
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let t = self.next_token();
            if t == Token::Eof {
                break;
            }
            tokens.push(t);
        }

        tokens
    }
}
