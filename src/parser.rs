use crate::lexer::*;
use std::fs;

#[derive(Debug, Clone)]
pub enum Expr {
    IntNumber(i64),          //i64
    FloatNumber(f64),        //f64
    IntSNumber(i32),         //i32
    FloatSNumber(f32),       //f32
    String(String),          //ptr:char=String
    Char(char),              //char
    Bool(bool),              //bool
    ArrayLiteral(Vec<Expr>), //Array(e1,e2,e3,...)
    Ident(String),           //変数名,型名,ワープに使うラベル名前などStringではない文字列
    Return(Vec<Expr>),       //関数の返り値
    Point(Vec<Expr>),        //ワープに使うラベル
    Call {
        name: String,
        args: Vec<Expr>,
    }, //関数呼び出し
    Warp {
        name: String,
        args: Vec<Expr>,
    }, //warpto,warptoif
    StructVal {
        _name: String,
        _args: Vec<Expr>,
    }, //structの実体
    TypeApply {
        base: String,
        args: Vec<Expr>,
    }, //ptr:typeとかのbase:type
    Loopif {
        name: String,
        cond: Vec<Expr>,
        body: Vec<Stmt>,
    }, //loopif:name(cond){do}
    If {
        branches: Vec<IfBranch>,
        else_block: Option<Vec<Stmt>>,
    },
}
#[derive(Debug, Clone)]
pub struct IfBranch {
    pub cond: Vec<Expr>,
    pub body: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub expr: Expr,
}
#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),
    Struct(Struct),
    Declare(Declare),
}
#[derive(Debug, Clone)]
pub struct Declare {
    pub name: String,
    pub return_type: Expr,
    pub args: Vec<Expr>,
    pub is_vararg: bool,
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
    pub _return_type: Expr,
    pub args: Vec<(Expr, Expr)>, //(type,name)
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<TopLevel>, //functions & structs & toplevel call
    pub has_main: bool,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    has_main: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            has_main: false,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn peek_back(&self) -> Option<&Token> {
        self.tokens.get(self.pos - 1)
    }
    fn _peek_front(&self) -> Option<&Token> {
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

    fn connect_use(&mut self, import_map: &mut Vec<String>, funcs: &mut Vec<TopLevel>) {
        self.no_return_next();
        let Token::StringLiteral(path) = self.next().unwrap_or(&Token::EOF) else {
            return;
        };
        let source = fs::read_to_string(path).expect("ファイルを読み込めませんでした");
        if import_map.contains(&path) {
            return;
        }
        let mut tokenizer = Tokenizer::new(source.as_str());
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse_program(import_map);
        for module_ast in parsed.functions {
            funcs.push(module_ast);
        }
        import_map.push(path.to_string());
    }
    // -------------------------
    // Parse entire program
    // -------------------------
    pub fn parse_program(&mut self, import_map: &mut Vec<String>) -> Program {
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
                _ => {
                    // toplevel eval
                    let expr = self.parse_expr();
                    match expr {
                        Expr::Call { name, args: _ } if name == "main" => {}
                        other => {
                            funcs.push(TopLevel::Function(Function {
                                name: "toplevel_child".to_string(),
                                return_type: Expr::Ident("void".to_string()),
                                args: vec![],
                                body: vec![Stmt { expr: other }],
                            }));
                        }
                    }
                }
            }
        }

        Program {
            functions: funcs,
            has_main: self.has_main,
        }
    }

    // -------------------------
    // Parse function
    // fn name():i32 { ... }
    // -------------------------
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

        // (type name,type name,...)
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
                Some(Token::Ident(s)) => self.parse_type_apply(s.clone()), //Expr::Ident(s.clone()),
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

        // (type name,type name,...)
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
            args: args,
            is_vararg,
        }
    }
    // -------------------------
    // Parse expression (function-call style)
    //
    //   +(1,2)
    //   =(a, 1, i32)
    //   println(a)
    //   warpto(label)
    //   loopif:name(cond){}
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

    // -------------------------
    // Function call parsing
    //
    // name(arg1, arg2, ...)
    // -------------------------
    fn parse_type_apply(&mut self, name: String) -> Expr {
        match self.next() {
            Some(Token::Colon) => {}
            _ => {
                self.no_return_back();
                return Expr::Ident(name.clone());
            } //normal types
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
    fn parse_if(&mut self) -> Expr {
        let mut has_else = false;
        let mut branches = Vec::new();
        let mut else_block = None;


        loop {
            match self.next() {
                Some(Token::Lsep(LSeparator::LParen)) => {}
                other =>if !has_else{panic!("expected '(' after if got {:?}", other);}else{self.no_return_back();} ,
            }

            let mut args = Vec::new();

            loop {
                if has_else{
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
            if has_else{
                else_block = Some(stmts);
                break;
            }else {
                branches.push(IfBranch { cond: args, body: stmts });
            }
            match self.next().unwrap() {
                Token::ElIf=>{},
                Token::Else=>{has_else = true;},
                _ => {self.no_return_back();break;}                
            }
            
        }

        Expr::If { branches, else_block}
    }
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
