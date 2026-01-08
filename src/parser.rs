use crate::lexer::*;
use std::fs;

/// Represents an expression in the WapL abstract syntax tree.
#[derive(Debug, Clone)]
pub enum Expr {
    /// isize integer literal
    IsizeNumber(isize),
    /// i64 integer literal
    IntNumber(i64),
    /// f64 float literal
    FloatNumber(f64),
    /// i32 integer literal
    IntSNumber(i32),
    /// f32 float literal
    FloatSNumber(f32),
    /// String literal (char pointer)
    String(String),
    /// Character literal
    Char(char),
    /// Boolean literal
    Bool(bool),
    /// Array literal: Array(e1, e2, ...)
    ArrayLiteral(Vec<Expr>),
    /// Identifier (variable names, types, labels, etc.)
    Ident(String),
    /// return expression
    Return(Vec<Expr>),
    /// label for warpto/warptoif
    Point(Vec<Expr>),
    /// Function call: name(arg1, arg2, ...)
    Call { name: String, args: Vec<Expr> },
    /// Warp jump: warpto(label) or warptoif(label)
    Warp { name: String, args: Vec<Expr> },
    /// Struct instantiation
    StructVal { _name: String, _args: Vec<Expr> },
    /// Type application (e.g., ptr:i32)
    TypeApply { base: String, args: Vec<Expr> },
    /// loopif:name(cond){ body }
    Loopif {
        name: String,
        cond: Vec<Expr>,
        body: Vec<Stmt>,
    },
    /// if or elif branch sequence
    If {
        branches: Vec<IfBranch>,
        else_block: Option<Vec<Stmt>>,
    },
}
/// A single branch of an `if` expression.
#[derive(Debug, Clone)]
pub struct IfBranch {
    pub cond: Vec<Expr>,
    pub body: Vec<Stmt>,
}
/// A statement in the WapL AST, which wraps an expression.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub expr: Expr,
}
/// Top-level declarations and expressions in a program.
#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),
    Struct(Struct),
    Declare(Declare),
    Export(Export),
}
/// External function declaration for FFI.
#[derive(Debug, Clone)]
pub struct Declare {
    pub name: String,
    pub return_type: Expr,
    pub args: Vec<Expr>,
    pub is_vararg: bool,
}
/// Export declaration to make a function visible internationally.
#[derive(Debug, Clone)]
pub struct Export {
    pub name: String,
}
/// Function definition.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Expr,
    pub args: Vec<(Expr, Expr)>, // (type, name)
    pub body: Vec<Stmt>,
}
/// Struct definition.
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub _return_type: Expr,
    pub args: Vec<(Expr, Expr)>, // (type, name)
}

/// Represents the entire parsed program.
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<TopLevel>, // functions, structs, and top-level expressions
    pub has_main: bool,
}

/// The WapL Parser.
pub struct Parser {
    /// Token stream to parse.
    tokens: Vec<Token>,
    /// Current position in the token stream.
    pos: usize,
    /// Whether a `main` function was found.
    has_main: bool,
    /// Counter for generating unique names for top-level expressions.
    toplevel_counter: usize,
}

impl Parser {
    /// Creates a new Parser.
    pub fn new(tokens: Vec<Token>, toplevel_counter: usize) -> Self {
        Self {
            tokens,
            pos: 0,
            has_main: false,
            toplevel_counter,
        }
    }

    /// Peeks at the current token.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    /// Peeks at the previous token.
    fn peek_back(&self) -> Option<&Token> {
        self.tokens.get(self.pos - 1)
    }

    /// Peeks at the token after the current one.
    fn _peek_front(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }

    /// Moves the position forward without returning a value.
    fn no_return_next(&mut self) {
        self.pos += 1;
    }

    /// Moves the position backward without returning a value.
    fn no_return_back(&mut self) {
        self.pos -= 1;
    }

    /// Returns the current token and advances the position.
    fn next(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;
        t
    }

    /// Expects the current token to be `expected`. Panics if not.
    fn expect(&mut self, expected: &Token) {
        let t = self.next().expect("unexpected EOF");
        if t != expected {
            panic!("expected {:?}, got {:?}", expected, t);
        }
    }

    /// Checks if the current token matches `expected`.
    fn check(&self, expected: &Token) -> bool {
        let t = self.peek().expect("unexpected EOF");
        t == expected
    }

    /// Consumes a comma if it's the current token.
    fn consume_comma(&mut self) {
        match self.peek() {
            Some(Token::Comma) => {
                self.next();
            }
            _ => {}
        }
    }

    /// Consumes a semicolon if it's the current token.
    fn consume_semicolon(&mut self) {
        match self.peek() {
            Some(Token::Semicolon) => {
                self.next();
            }
            _ => {}
        }
    }

    /// Connects a `use` or `import` statement by parsing the referenced file.
    fn connect_use(&mut self, import_map: &mut Vec<String>, funcs: &mut Vec<TopLevel>) {
        self.no_return_next();
        let mut top_count = self.toplevel_counter;
        let Token::StringLiteral(path) = self.next().unwrap_or(&Token::EOF) else {
            return;
        };
        let source = fs::read_to_string(path).expect("Could not read file");
        if import_map.contains(&path) {
            return;
        }
        let mut tokenizer = Tokenizer::new(source.as_str());
        let tokens = tokenizer.tokenize();

        let mut parser = Parser::new(tokens, top_count);
        let (parsed, temp_top_count) = parser.parse_program(import_map);
        top_count = temp_top_count;
        for module_ast in parsed.functions {
            funcs.push(module_ast);
        }

        import_map.push(path.to_string());
        self.toplevel_counter = top_count;
    }

    /// Parses the entire program into a `Program` AST.
    pub fn parse_program(&mut self, import_map: &mut Vec<String>) -> (Program, usize) {
        let mut funcs = Vec::new();

        while let Some(tok) = self.peek() {
            match tok {
                Token::Import => {
                    self.connect_use(import_map, &mut funcs);
                }
                Token::Fn => {
                    funcs.push(TopLevel::Function(self.parse_function()));
                }
                Token::Struct => {
                    funcs.push(TopLevel::Struct(self.parse_struct()));
                }
                Token::Declare => {
                    funcs.push(TopLevel::Declare(self.parse_declare()));
                }
                Token::Semicolon => {
                    self.no_return_next();
                }
                Token::Export => {
                    funcs.push(TopLevel::Export(self.parse_export()));
                }
                _ => {
                    // toplevel evaluation
                    let expr = self.parse_expr();
                    match expr {
                        Expr::Call { name, args: _ } if name == "main" => {}
                        other => {
                            funcs.push(TopLevel::Function(Function {
                                name: format!("toplevel_child.{}", self.toplevel_counter),
                                return_type: Expr::Ident("void".to_string()),
                                args: vec![],
                                body: vec![Stmt { expr: other }],
                            }));
                            self.toplevel_counter += 1;
                        }
                    }
                }
            }
        }

        (
            Program {
                functions: funcs,
                has_main: self.has_main,
            },
            self.toplevel_counter,
        )
    }

    /// Parses a function definition.
    /// Syntax: fn name(arg1 arg_type1, ...): return_type { body }
    fn parse_function(&mut self) -> Function {
        self.expect(&Token::Fn);

        let mut is_main = false;
        // function name
        let mut name = match self.next() {
            Some(Token::Ident(s)) => s.clone(),
            other => panic!("expected function name, got {:?}", other),
        };
        name = if name == "main" {
            is_main = true;
            self.has_main = true;
            "main".to_string()
        } else {
            name
        };

        // (type name, type name, ...)
        self.expect(&Token::Lsep(LSeparator::LParen));
        let mut args = Vec::new();
        while let Some(token_arg_type) = self.peek() {
            if *token_arg_type == Token::Rsep(RSeparator::RParen) {
                break;
            }
            self.no_return_next();
            let arg_type = match self.peek_back() {
                Some(Token::Ident(s)) => self.parse_type_apply(s.clone()),
                other => panic!("expected arg type, got {:?}", other),
            };
            let arg_name = match self.next() {
                Some(Token::Ident(s)) => Expr::Ident(s.clone()),
                other => panic!("expected arg name, got {:?}", other),
            };
            args.push((arg_type, arg_name));
            self.consume_comma();
        }
        self.expect(&Token::Rsep(RSeparator::RParen));

        let mut return_type = Expr::Ident("void".to_string());
        if self.check(&Token::Colon) {
            self.expect(&Token::Colon);
            if !self.check(&Token::Lsep(LSeparator::LBrace)) {
                return_type = self.parse_return_type();
            }
        }

        // { statements }
        self.expect(&Token::Lsep(LSeparator::LBrace));
        self.consume_semicolon();

        let mut stmts = Vec::new();
        if is_main {
            // Include automatic top-level call in main
            stmts.push(Stmt {
                expr: Expr::Call {
                    name: "_TOPLEVEL_".to_string(),
                    args: vec![],
                },
            })
        }

        while let Some(tok) = self.peek() {
            if *tok == Token::Rsep(RSeparator::RBrace) {
                break;
            }
            // optional ;
            if let Some(Token::Semicolon) = self.peek() {
                self.next();
            }

            let expr = self.parse_expr();
            stmts.push(Stmt { expr });

            // optional ;
            if let Some(Token::Semicolon) = self.peek() {
                self.next();
            }
        }

        self.expect(&Token::Rsep(RSeparator::RBrace));
        self.consume_semicolon();

        Function {
            name,
            return_type,
            args,
            body: stmts,
        }
    }
    /// Parses a struct definition.
    /// Syntax: struct name { type field1, type field2, ... }
    fn parse_struct(&mut self) -> Struct {
        self.expect(&Token::Struct);

        // struct name
        let (name, return_type) = match self.next() {
            Some(Token::Ident(s)) => (s.clone(), Expr::Ident(s.clone())),
            other => panic!("expected struct name, got {:?}", other),
        };
        self.expect(&Token::Lsep(LSeparator::LBrace));
        self.consume_semicolon();
        let mut args = Vec::new();
        while let Some(token_arg_type) = self.peek() {
            if *token_arg_type == Token::Rsep(RSeparator::RBrace) {
                break;
            }
            self.no_return_next();
            let arg_type = match self.peek_back() {
                Some(Token::Ident(s)) => self.parse_type_apply(s.clone()),
                other => panic!("expected arg type, got {:?}", other),
            };
            let arg_name = match self.next() {
                Some(Token::Ident(s)) => Expr::Ident(s.clone()),
                other => panic!("expected arg name, got {:?}", other),
            };
            args.push((arg_type, arg_name));
            self.consume_comma();
        }
        self.expect(&Token::Rsep(RSeparator::RBrace));
        Struct {
            name,
            _return_type: return_type,
            args,
        }
    }

    /// Parses an export declaration.
    fn parse_export(&mut self) -> Export {
        self.expect(&Token::Export);
        let name = match self.next() {
            Some(Token::Ident(s)) => s.clone(),
            other => panic!("expected function name, got {:?}", other),
        };
        self.consume_semicolon();
        Export { name }
    }

    /// Parses an external function declaration.
    fn parse_declare(&mut self) -> Declare {
        self.expect(&Token::Declare);

        // function name
        let mut name = match self.next() {
            Some(Token::Ident(s)) => s.clone(),
            other => panic!("expected function name, got {:?}", other),
        };
        name = if name == "main" {
            "main".to_string()
        } else {
            name
        };

        // (type arg1, type arg2, ...)
        self.expect(&Token::Lsep(LSeparator::LParen));
        let mut args = Vec::new();
        let mut is_vararg = false;
        while let Some(token_arg_type) = self.peek() {
            if *token_arg_type == Token::Rsep(RSeparator::RParen) {
                break;
            }
            self.no_return_next();
            let arg_type = match self.peek_back() {
                Some(Token::Ident(s)) => self.parse_type_apply(s.clone()),
                other => panic!("expected arg type, got {:?}", other),
            };
            match arg_type {
                Expr::Ident(s) if s == "..." => {
                    is_vararg = true;
                }
                _ => {
                    args.push(arg_type);
                }
            }
            self.consume_comma();
        }
        self.expect(&Token::Rsep(RSeparator::RParen));

        let mut return_type = Expr::Ident("void".to_string());
        if self.check(&Token::Colon) {
            self.expect(&Token::Colon);
            if !self.check(&Token::Lsep(LSeparator::LBrace)) {
                return_type = self.parse_return_type();
            }
        }
        self.consume_semicolon();

        self.consume_semicolon();

        Declare {
            name,
            return_type,
            args,
            is_vararg,
        }
    }
    /// Parses a return type or a simple type identifier.
    fn parse_return_type(&mut self) -> Expr {
        self.no_return_next();
        let token = self.peek_back();
        match token {
            Some(Token::Ident(name)) => {
                let front = self.peek();
                match front {
                    Some(Token::Colon) => self.parse_type_apply(name.clone()),
                    Some(_) => Expr::Ident(name.clone()),
                    _ => panic!("unexpected EOF in expression"),
                }
            }
            Some(tok) => panic!(
                "{}expression begins with unexpected token {:?}",
                self.pos, tok
            ),
            None => panic!("unexpected EOF in expression"),
        }
    }

    /// The main entry point for parsing an expression.
    fn parse_expr(&mut self) -> Expr {
        self.no_return_next();
        let token = self.peek_back();
        match token {
            Some(Token::IntNumber(n)) => Expr::IntNumber(*n),
            Some(Token::IsizeNumber(n)) => Expr::IsizeNumber(*n),
            Some(Token::FloatNumber(n)) => Expr::FloatNumber(*n),
            Some(Token::IntshortNumber(n)) => Expr::IntSNumber(*n),
            Some(Token::FloatshortNumber(n)) => Expr::FloatSNumber(*n),
            Some(Token::StringLiteral(s)) => Expr::String(s.clone()),
            Some(Token::CharLiteral(c)) => Expr::Char(*c),
            Some(Token::BoolLiteral(b)) => Expr::Bool(*b),
            Some(Token::Return) => Expr::Return(vec![self.parse_expr()]),
            Some(Token::Point) => Expr::Point(vec![self.parse_expr()]),
            Some(Token::Warpto) => self.parse_call("warpto".to_string()),
            Some(Token::WarptoIf) => self.parse_call("warptoif".to_string()),
            Some(Token::LoopIf) => self.parse_loopif(),
            Some(Token::If) => self.parse_if(),
            Some(Token::ArrayCall) => self.parse_call("Array".to_string()),
            Some(Token::Ident(name)) => self.parse_ident_expr(name.clone()),
            Some(tok) => panic!(
                "{}expression begins with unexpected token {:?}",
                self.pos, tok
            ),
            None => panic!("unexpected EOF in expression"),
        }
    }
    /// Dispatches identifier expressions to specific parsers (call, struct, type, or simple ident).
    fn parse_ident_expr(&mut self, name: String) -> Expr {
        let front = self.peek();
        match front {
            Some(Token::Lsep(LSeparator::LParen)) => self.parse_call(name.clone()),
            Some(Token::Lsep(LSeparator::LBrace)) => self.parse_structval(name.clone()),
            Some(Token::Colon) => self.parse_type_apply(name.clone()),
            Some(_) => Expr::Ident(name.clone()),
            _ => panic!("unexpected EOF in expression"),
        }
    }

    /// Parses a type application.
    /// Syntax: base:type or base (for primitive types)
    fn parse_type_apply(&mut self, name: String) -> Expr {
        match self.next() {
            Some(Token::Colon) => {}
            _ => {
                self.no_return_back();
                if name.as_str() == "str" {
                    return Expr::TypeApply {
                        base: "ptr".to_string(),
                        args: vec![Expr::Ident("char".to_string())],
                    };
                }
                return Expr::Ident(name.clone());
            } // normal types
        }
        let mut args = Vec::new();
        let arg = self.parse_return_type();
        args.push(arg);
        self.consume_comma();

        Expr::TypeApply { base: name, args }
    }
    /// Parses a struct literal.
    /// Syntax: name { val1, val2, ... }
    fn parse_structval(&mut self, name: String) -> Expr {
        match self.next() {
            Some(Token::Lsep(LSeparator::LBrace)) => {}
            other => panic!(
                "expected '{{' after function name {:?}, got {:?}",
                name, other
            ),
        }
        let mut args = Vec::new();

        loop {
            match self.peek() {
                Some(Token::Rsep(RSeparator::RBrace)) => {
                    self.next();
                    break;
                }
                _ => {
                    let arg = self.parse_expr();
                    args.push(arg);
                    self.consume_comma();
                }
            }
        }
        Expr::StructVal {
            _name: name,
            _args: args,
        }
    }

    /// Parses a function call.
    /// Syntax: name(arg1, arg2, ...)
    fn parse_call(&mut self, mut name: String) -> Expr {
        match self.next() {
            Some(Token::Lsep(LSeparator::LParen)) => {}
            other => panic!(
                "expected '(' after function name {:?}, got {:?}",
                name, other
            ),
        }

        let mut args = Vec::new();

        loop {
            match self.peek() {
                Some(Token::Rsep(RSeparator::RParen)) => {
                    self.next();
                    break;
                }
                _ => {
                    let arg = self.parse_expr();
                    args.push(arg);
                    self.consume_comma();
                }
            }
        }
        if name == "warpto" || name == "warptoif" {
            self.consume_semicolon();
            return Expr::Warp { name, args };
        } else if name == "Array" {
            self.consume_semicolon();
            return Expr::ArrayLiteral(args);
        }
        name = if name == "main" {
            "main".to_string()
        } else {
            name
        };
        self.consume_semicolon();
        Expr::Call { name, args }
    }
    /// Parses an `if` expression including `elif` and `else` branches.
    fn parse_if(&mut self) -> Expr {
        let mut has_else = false;
        let mut branches = Vec::new();
        let mut else_block = None;

        loop {
            match self.next() {
                Some(Token::Lsep(LSeparator::LParen)) => {}
                other => {
                    if !has_else {
                        panic!("expected '(' after if got {:?}", other);
                    } else {
                        self.no_return_back();
                    }
                }
            }

            let mut args = Vec::new();

            loop {
                if has_else {
                    break;
                }
                match self.peek() {
                    Some(Token::Rsep(RSeparator::RParen)) => {
                        self.next();
                        break;
                    }
                    _ => {
                        let arg = self.parse_expr();
                        args.push(arg);
                        self.consume_comma();
                    }
                }
            }

            // { statements }
            self.expect(&Token::Lsep(LSeparator::LBrace));
            self.consume_semicolon();

            let mut stmts = Vec::new();

            while let Some(tok) = self.peek() {
                if *tok == Token::Rsep(RSeparator::RBrace) {
                    break;
                }
                // optional ;
                if let Some(Token::Semicolon) = self.peek() {
                    self.next();
                }

                let expr = self.parse_expr();
                stmts.push(Stmt { expr });

                // optional ;
                if let Some(Token::Semicolon) = self.peek() {
                    self.next();
                }
            }

            self.expect(&Token::Rsep(RSeparator::RBrace));
            self.consume_semicolon();
            if has_else {
                else_block = Some(stmts);
                break;
            } else {
                branches.push(IfBranch {
                    cond: args,
                    body: stmts,
                });
            }
            match self.next().unwrap() {
                Token::ElIf => {}
                Token::Else => {
                    has_else = true;
                }
                _ => {
                    self.no_return_back();
                    break;
                }
            }
        }

        Expr::If {
            branches,
            else_block,
        }
    }

    /// Parses a `loopif` expression.
    fn parse_loopif(&mut self) -> Expr {
        self.expect(&Token::Colon);

        // loopif name
        let name = match self.next() {
            Some(Token::Ident(s)) => s.clone(),
            _ => {
                self.no_return_back();
                "loop".to_string()
            }
        };

        // (cond)
        match self.next() {
            Some(Token::Lsep(LSeparator::LParen)) => {}
            other => panic!("expected '(' after loopif name {:?}, got {:?}", name, other),
        }

        let mut args = Vec::new();

        loop {
            match self.peek() {
                Some(Token::Rsep(RSeparator::RParen)) => {
                    self.next();
                    break;
                }
                _ => {
                    let arg = self.parse_expr();
                    args.push(arg);
                    self.consume_comma();
                }
            }
        }

        // { statements }
        self.expect(&Token::Lsep(LSeparator::LBrace));
        self.consume_semicolon();

        let mut stmts = Vec::new();

        while let Some(tok) = self.peek() {
            if *tok == Token::Rsep(RSeparator::RBrace) {
                break;
            }
            // optional ;
            if let Some(Token::Semicolon) = self.peek() {
                self.next();
            }

            let expr = self.parse_expr();
            stmts.push(Stmt { expr });

            // optional ;
            if let Some(Token::Semicolon) = self.peek() {
                self.next();
            }
        }

        self.expect(&Token::Rsep(RSeparator::RBrace));
        self.consume_semicolon();

        Expr::Loopif {
            name,
            cond: args,
            body: stmts,
        }
    }
}
