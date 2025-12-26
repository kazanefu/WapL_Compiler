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
use std::env;

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

    /// bit数(isizeのサイズ)
    #[arg(long, default_value = "64")]
    bitsize: String,

    /// WASM build
    #[arg(long)]
    wasm: bool,

    /// sysroot
    #[arg(
        long,
        default_value = "$HOME/wasi-sdk-29.0-x86_64-linux/share/wasi-sysroot"
    )]
    sysroot: String,

    /// wasm2wat のパス (環境PATHに通ってる場合は不要)
    #[arg(long, default_value = "wasm2wat")]
    wasm2wat: String,

    /// output wat
    #[arg(long, default_value = "a.wat")]
    wat: String,
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
        let mut codegen = Codegen::new(&context, "wapl_module", args.bitsize.clone());
        codegen.compile_program(parsed);

        // 出力.ll名
        let ll_filename = filename.replace(".wapl", ".ll");
        codegen.module.print_to_file(&ll_filename).unwrap();

        println!("LLVM IR output: {}", ll_filename);

        if !Path::new(&ll_filename).exists() {
            return Err(format!("LLVM IR file not found: {}", ll_filename).into());
        }
        if !args.ir {
            let clang_path = args.clang.replace("$HOME", &env::var("HOME")?);
            let status = Command::new(&clang_path)
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
        if args.wasm {
            let sysroot_path = args.sysroot.replace("$HOME", &env::var("HOME")?);
            let clang_path = args.clang.replace("$HOME", &env::var("HOME")?);
            // ① clang
            let status = Command::new(&clang_path)
                .args([
                    "--target=wasm32-wasi",
                    &format!("--sysroot={}", sysroot_path),
                    &format!("-{}", args.opt_level),
                    &ll_filename,
                    "-o",
                    &args.output,
                ])
                .status()?;

            if !status.success() {
                return Err("wasm clang failed to compile".into());
            }

            let wasm2wat_path = args.wasm2wat.replace("$HOME", &env::var("HOME")?);

            // ② wasm2wat
            let status = Command::new(&wasm2wat_path)
                .args([&args.output, "-o", &args.wat])
                .status()?;

            if !status.success() {
                return Err("wasm2wat failed".into());
            }

            println!("WASM Build success!");
            println!("  wasm → {}", args.output);
            println!("  wat  → {}", args.wat);
        }

        Ok(())
    } else {
        println!("コンパイルするファイルを指定してください");
        return Err(format!("Input file not found: {}", args.input).into());
    }
}
