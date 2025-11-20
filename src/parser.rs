use crate::lexer::*;
use std::fs;

#[derive(Debug, Clone)]
pub enum Expr {
    IntNumber(i64),
    FloatNumber(f64),
    String(String),
    Char(char),
    Bool(bool),
    ArrayLiteral(Vec<Expr>),
    Ident(String),     //変数名,型名,ワープに使うラベル名前などStringではない文字列
    Return(Vec<Expr>), //関数の返り値
    Point(Vec<Expr>),  //ワープに使うラベル
    Call { name: String, args: Vec<Expr> }, //関数呼び出し
    Warp { name: String, args: Vec<Expr> }, //warpto,warptoif
    StructVal { name: String, args: Vec<Expr> }, //structの実体
    TypeApply { base: String, args: Vec<Expr> }, //ptr:typeとかのbase:type
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),
    Struct(Struct),
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Expr,
    pub args: Vec<(Expr, Expr)>, //(type,name)
    pub body: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub return_type: Expr,
    pub args: Vec<(Expr, Expr)>, //(type,name)
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<TopLevel>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn peek_back(&self) -> Option<&Token> {
        self.tokens.get(self.pos - 1)
    }
    fn peek_front(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }
    fn no_return_next(&mut self) {
        self.pos += 1;
    }
    fn no_return_back(&mut self) {
        self.pos -= 1;
    }
    fn next(&mut self) -> Option<&Token> {
        let t = self.tokens.get(self.pos);
        self.pos += 1;
        t
    }

    fn expect(&mut self, expected: &Token) {
        let t = self.next().expect("unexpected EOF");
        if t != expected {
            panic!("expected {:?}, got {:?}", expected, t);
        }
    }
    fn check(&self, expected: &Token) -> bool {
        let t = self.peek().expect("unexpected EOF");
        t == expected
    }

    fn consume_comma(&mut self) {
        match self.peek() {
            Some(Token::Comma) => {
                self.next();
            }
            _ => {}
        }
    }
    fn consume_semicolon(&mut self) {
        match self.peek() {
            Some(Token::Semicolon) => {
                self.next();
            }
            _ => {}
        }
    }

    // -------------------------
    // Parse entire program
    // -------------------------
    pub fn parse_program(&mut self, import_map: &mut Vec<String>) -> Program {
        let mut funcs = Vec::new();

        while let Some(tok) = self.peek() {
            match tok {
                Token::Import => {
                    self.no_return_next();
                    match self.next() {
                        Some(Token::StringLiteral(s)) => {
                            let source =
                                fs::read_to_string(s).expect("ファイルを読み込めませんでした");
                            if !import_map.contains(&s) {
                                let mut tokenizer = Tokenizer::new(source.as_str());
                                let tokens = tokenizer.tokenize();
                                let mut parser = Parser::new(tokens);
                                let parsed = parser.parse_program(import_map);
                                for module_ast in parsed.functions {
                                    funcs.push(module_ast);
                                }
                                import_map.push(s.to_string());
                            }
                        }
                        _ => {}
                    }
                }
                Token::Fn => {
                    funcs.push(TopLevel::Function(self.parse_function()));
                }
                Token::Struct => {
                    funcs.push(TopLevel::Struct(self.parse_struct()));
                }
                Token::Semicolon => {
                    self.no_return_next();
                }
                _ => {
                    // トップレベルに式も許可する
                    let expr = self.parse_expr();
                    funcs.push(TopLevel::Function(Function {
                        name: "toplevel_child".to_string(),
                        return_type: Expr::Ident("void".to_string()),
                        args: vec![],
                        body: vec![Stmt { expr }],
                    }));
                }
            }
        }

        Program { functions: funcs }
    }

    // -------------------------
    // Parse function
    // fn name():i32 {; ... };
    // -------------------------
    fn parse_function(&mut self) -> Function {
        self.expect(&Token::Fn);

        // name
        let mut name = match self.next() {
            Some(Token::Ident(s)) => s.clone(),
            other => panic!("expected function name, got {:?}", other),
        };
        name = if name == "main" {
            "user_main".to_string()
        } else {
            name
        };

        // ()
        self.expect(&Token::Lsep(LSeparator::LParen));
        let mut args = Vec::new();
        while let Some(token_arg_type) = self.peek() {
            if *token_arg_type == Token::Rsep(RSeparator::RParen) {
                break;
            }
            self.no_return_next();
            let arg_type = match self.peek_back() {
                Some(Token::Ident(s)) => self.parse_type_apply(s.clone()), //Expr::Ident(s.clone()),
                other => panic!("expected arg type, got {:?}", other),
            };
            println!("{:?}", arg_type);
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
        self.consume_semicolon(); //self.expect(&Token::Semicolon);

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
        self.consume_semicolon(); //self.expect(&Token::Semicolon);

        Function {
            name,
            return_type,
            args: args,
            body: stmts,
        }
    }
    // -------------------------
    // Parse struct
    // struct name{ i32 a, ptr b,..., }
    //
    fn parse_struct(&mut self) -> Struct {
        self.expect(&Token::Struct);

        // name
        let (name, return_type) = match self.next() {
            Some(Token::Ident(s)) => (s.clone(), Expr::Ident(s.clone())),
            other => panic!("expected function name, got {:?}", other),
        };
        self.expect(&Token::Lsep(LSeparator::LBrace));
        self.consume_semicolon();
        let mut args = Vec::new();
        while let Some(token_arg_type) = self.peek() {
            if *token_arg_type == Token::Rsep(RSeparator::RBrace) {
                break;
            }
            let arg_type = match self.next() {
                Some(Token::Ident(s)) => Expr::Ident(s.clone()),
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
            return_type,
            args,
        }
    }

    // -------------------------
    // Parse expression (function-call style)
    //
    //   +(1,2)
    //   =(a, 1, i32)
    //   println(a)
    //   warpto(label)
    // -------------------------
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
    fn parse_expr(&mut self) -> Expr {
        self.no_return_next();
        let token = self.peek_back();
        match token {
            Some(Token::IntNumber(n)) => Expr::IntNumber(*n),
            Some(Token::FloatNumber(n)) => Expr::FloatNumber(*n),
            Some(Token::StringLiteral(s)) => Expr::String(s.clone()),
            Some(Token::CharLiteral(c)) => Expr::Char(*c),
            Some(Token::BoolLiteral(b)) => Expr::Bool(*b),
            Some(Token::Return) => Expr::Return(vec![self.parse_expr()]),
            Some(Token::Point) => Expr::Point(vec![self.parse_expr()]),
            Some(Token::Warpto) => self.parse_call("warpto".to_string()),
            Some(Token::WarptoIf) => self.parse_call("warptoif".to_string()),
            Some(Token::ArrayCall) => self.parse_call("Array".to_string()),
            Some(Token::Ident(name)) => {
                let front = self.peek();
                match front {
                    Some(Token::Lsep(LSeparator::LParen)) => self.parse_call(name.clone()),
                    Some(Token::Lsep(LSeparator::LBrace)) => self.parse_structval(name.clone()),
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

    // -------------------------
    // Function call parsing
    //
    // name(arg1, arg2, ...)
    // -------------------------
    fn parse_type_apply(&mut self, name: String) -> Expr {
        match self.next() {
            Some(Token::Colon) => {}
            _=> {self.no_return_back();return Expr::Ident(name.clone());}
            // other => panic!(
            //     "expected ':' after type name {:?}, got {:?}",
            //     name, other
            // ),
        }
        let mut args = Vec::new();
        match self.peek() {
            _ => {
                let arg = self.parse_return_type();
                args.push(arg);
                self.consume_comma();
            }
        }
        Expr::TypeApply { base: name, args }
    }
    fn parse_structval(&mut self, name: String) -> Expr {
        match self.next() {
            Some(Token::Lsep(LSeparator::LBrace)) => {}
            other => panic!(
                "expected '(' after function name {:?}, got {:?}",
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
        Expr::StructVal { name, args }
    }
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
            "user_main".to_string()
        } else {
            name
        };
        self.consume_semicolon();
        Expr::Call { name, args }
    }
}
