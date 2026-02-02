mod codegen;
mod lexer;
mod parser;

use clap::Parser;
use codegen::*;
use inkwell::context::Context;
use lexer::*;
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

/// WapL Compiler (generates LLVM IR and compiles it to an executable using clang)
#[derive(clap::Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input WapL (.wapl) file
    #[arg(short, long)]
    input: String,

    /// Output filename
    #[arg(short, long, default_value = "a.out")]
    output: String,

    /// Optimization level (O0, O1, O2, O3, Os, Oz)
    #[arg(short = 'O', long = "opt", default_value = "O2")]
    opt_level: String,

    /// Path to clang (not needed if in PATH)
    #[arg(long, default_value = "clang")]
    clang: String,

    /// Display tokens and AST for debugging
    #[arg(short, long)]
    debug: bool,

    /// Generate LLVM IR only (no executable)
    #[arg(long)]
    ir: bool,

    /// bit size (size of isize)
    #[arg(long, default_value = "64")]
    bitsize: String,

    /// WASM build
    #[arg(long)]
    wasm: bool,

    /// WASM browser build
    #[arg(long)]
    browser: bool,

    /// sysroot for WASI
    #[arg(
        long,
        default_value = "$HOME/wasi-sdk-29.0-x86_64-linux/share/wasi-sysroot"
    )]
    sysroot: String,

    /// Path to wasm2wat (not needed if in PATH)
    #[arg(long, default_value = "wasm2wat")]
    wasm2wat: String,

    /// output wat filename
    #[arg(long, default_value = "a.wat")]
    wat: String,

    /// Initial memory size for WASM
    #[arg(long, default_value = "655360")]
    memory_size: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    if Path::new(&args.input).exists() {
        // Read file
        let filename = &args.input;
        let source = fs::read_to_string(filename).expect("Could not read file");

        // Lexical analysis
        let mut tokenizer = Tokenizer::new(source.as_str());
        let tokens = tokenizer.tokenize();

        // Output tokens for debugging
        if args.debug {
            println!("Tokens");
            for (j, i) in tokens.iter().enumerate() {
                println!("{j}:{:?}", i);
            }
        }

        // Parsing
        let mut import_map: Vec<String> = Vec::new();
        let mut parser = parser::Parser::new(tokens, 0);
        let (parsed, _) = parser.parse_program(&mut import_map);

        // Output AST for debugging
        if args.debug {
            println!("AST");
            for (j, i) in parsed.functions.iter().enumerate() {
                println!("{j}:{:?}", i);
            }
        }

        // IR generation
        let context = Context::create();
        let mut codegen = Codegen::new(
            &context,
            "wapl_module",
            args.bitsize.clone(),
            args.wasm,
            args.browser,
        );
        codegen.compile_program(parsed);

        // Output .ll filename
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
            if args.browser {
                let clang_path = args.clang.replace("$HOME", &env::var("HOME")?);
                // 1. clang
                let status = Command::new(&clang_path)
                    .args([
                        "--target=wasm32-unknown-unknown",
                        "-Wl,--no-entry",
                        &format!("-Wl,--initial-memory={}", args.memory_size),
                        "-nostdlib",
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

                // 2. wasm2wat
                let status = Command::new(&wasm2wat_path)
                    .args([&args.output, "-o", &args.wat])
                    .status()?;

                if !status.success() {
                    return Err("wasm2wat failed".into());
                }

                println!("WASM Build success!");
                println!("  wasm → {}", args.output);
                println!("  wat  → {}", args.wat);
            } else {
                let sysroot_path = args.sysroot.replace("$HOME", &env::var("HOME")?);
                let clang_path = args.clang.replace("$HOME", &env::var("HOME")?);
                // 1. clang
                let status = Command::new(&clang_path)
                    .args([
                        "--target=wasm32-wasi",
                        &format!("--sysroot={}", sysroot_path),
                        &format!("-Wl,--initial-memory={}", args.memory_size),
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

                // 2. wasm2wat
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
        }

        Ok(())
    } else {
        println!("Please specify a file to compile");
        Err(format!("Input file not found: {}", args.input).into())
    }
}
