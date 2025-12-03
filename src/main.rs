mod codegen;
mod lexer;
mod parser;

use clap::Parser;
use codegen::*;
use inkwell::context::Context;
use lexer::*;
use std::fs;
use std::path::Path;
use std::process::Command;

/// WapLコンパイラ(llvm irを作ってそれをclangで実行ファイルにする)
#[derive(clap::Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// 入力するWapL (.wapl) ファイル
    #[arg(short, long)]
    input: String,

    /// 出力ファイル名
    #[arg(short, long, default_value = "a.out")]
    output: String,

    /// 最適化レベル (O0, O1, O2, O3, Os, Oz)
    #[arg(short = 'O', long = "opt", default_value = "O2")]
    opt_level: String,

    /// clang のパス (環境PATHに通ってる場合は不要)
    #[arg(long, default_value = "clang")]
    clang: String,
    /// デバッグ用にトークン列とASTを表示
    #[arg(short, long)]
    debug: bool,

    /// 実行ファイルなし
    #[arg(long)]
    ir: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    if Path::new(&args.input).exists() {
        //ファイル読み込み
        let filename = &args.input;
        let source = fs::read_to_string(filename).expect("ファイルを読み込めませんでした");
        //字句解析
        let mut tokenizer = Tokenizer::new(source.as_str());
        let tokens = tokenizer.tokenize();
        //デバッグ用にトークン出力
        if args.debug {
            println!("Tokens");
            let mut j = 0;
            for i in &tokens {
                println!("{j}:{:?}", i);
                j += 1;
            }
        }

        //構文解析
        let mut import_map: Vec<String> = Vec::new();
        let mut parser = parser::Parser::new(tokens);
        let parsed = parser.parse_program(&mut import_map);
        //デバッグ用にAST出力
        if args.debug {
            println!("AST");
            let mut j = 0;
            for i in &parsed.functions {
                println!("{j}:{:?}", i);
                j += 1;
            }
        }

        //IR作成
        let context = Context::create();
        let mut codegen = Codegen::new(&context, "wapl_module");
        codegen.compile_program(parsed);

        // 出力.ll名
        let ll_filename = filename.replace(".wapl", ".ll");
        codegen.module.print_to_file(&ll_filename).unwrap();

        println!("LLVM IR output: {}", ll_filename);

        if !Path::new(&ll_filename).exists() {
            return Err(format!("LLVM IR file not found: {}", ll_filename).into());
        }
        if !args.ir {
            let status = Command::new(&args.clang)
                .args([
                    &ll_filename,
                    "-o",
                    &args.output,
                    &format!("-{}", args.opt_level),
                ])
                .status()?;

            if !status.success() {
                return Err("clang failed to compile".into());
            }

            println!("Build success! → {}", args.output);
        }

        Ok(())
    } else {
        println!("コンパイルするファイルを指定してください");
        return Err(format!("Input file not found: {}", args.input).into());
    }
}
