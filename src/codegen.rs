use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FloatType;
use inkwell::types::*;
use inkwell::values::*;
use std::collections::HashMap;

use crate::parser::*;

struct PendingJump<'ctx> {
    from: BasicBlock<'ctx>,
}

struct FunctionContext<'ctx> {
    function: FunctionValue<'ctx>, // ← 必須！
    labels: HashMap<String, BasicBlock<'ctx>>,
    unresolved: HashMap<String, Vec<PendingJump<'ctx>>>,
}

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub struct_types: HashMap<String, StructType<'ctx>>,
    pub struct_fields: HashMap<String, Vec<(String, BasicTypeEnum<'ctx>, u32)>>,
    str_counter: usize,
    current_fn: Option<FunctionContext<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        let this = Self {
            context,
            module,
            builder,
            struct_types: HashMap::new(),
            struct_fields: HashMap::new(),
            str_counter: 0,
            current_fn: None, //Some(FunctionContext { labels: HashMap::new() ,unresolved:HashMap::new()}),
        };
        this.init_external_functions(); // ← ここで登録！  
        this
    }

    pub fn compile_program(&mut self, program: Program) {
        for func in program.functions {
            match func {
                TopLevel::Function(f) => {
                    self.compile_function(f);
                }
                TopLevel::Struct(s) => {
                    self.compile_struct(s);
                }
            }
        }
        combine_toplevel(&self.module, &self.builder);
    }

    fn compile_function(&mut self, func: Function) {
        // --- 戻り値型 ---
        let return_type_is_void = matches!(func.return_type, Expr::Ident(ref s) if s == "void");

        let return_type_enum = if return_type_is_void {
            None
        } else {
            Some(self.llvm_type_from_expr(&func.return_type))
            // Some(match &func.return_type {
            //     Expr::Ident(name) => {
            //         if let Some(st) = self.struct_types.get(name) {
            //             st.as_basic_type_enum()
            //         } else {
            //             match name.as_str() {
            //                 "i32" => self.context.i32_type().into(),
            //                 "i64" => self.context.i64_type().into(),
            //                 "f32" => self.context.f32_type().into(),
            //                 "f64" => self.context.f64_type().into(),
            //                 "bool" => self.context.bool_type().into(),
            //                 "char" => self.context.i8_type().into(),
            //                 _ => panic!("Unknown return type: {}", name),
            //             }
            //         }
            //     }
            //     _ => panic!("Return type must be identifier"),
            // })
        };

        // --- 引数型 ---
        let arg_types: Vec<BasicTypeEnum> = func
            .args
            .iter()
            .map(|(ty, _)| self.llvm_type_from_expr(ty))
            .collect();

        // --- Metadata 型に変換 ---
        let arg_types_meta: Vec<BasicMetadataTypeEnum> =
            arg_types.iter().map(|t| (*t).into()).collect();

        // --- LLVM 関数型作成 ---
        let fn_type = if return_type_is_void {
            self.context.void_type().fn_type(&arg_types_meta, false)
        } else {
            return_type_enum.unwrap().fn_type(&arg_types_meta, false)
        };

        // --- 関数追加 ---
        let llvm_func = self.module.add_function(&func.name, fn_type, None);
        let entry = self.context.append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(entry);

        // --- 引数を alloca して初期化 ---
        let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();
        for (i, (_ty, arg_expr)) in func.args.iter().enumerate() {
            let param = llvm_func.get_nth_param(i as u32).unwrap();

            // 名前を取り出す（Expr::Ident 前提）
            let arg_name = match arg_expr {
                Expr::Ident(name) => name.as_str(),
                _ => panic!("Function argument name must be identifier"),
            };
            param.set_name(arg_name);

            // struct 型でも基本は alloca して格納
            let alloca = self
                .builder
                .build_alloca(param.get_type(), arg_name)
                .expect("alloca failed");
            self.builder.build_store(alloca, param).unwrap();
            variables.insert(arg_name.to_string(), alloca);
        }
        //戻り値をalloca
        let ret_alloca = if !return_type_is_void {
            Some(
                self.builder
                    .build_alloca(return_type_enum.unwrap(), "ret_val"),
            )
        } else {
            None
        };

        self.current_fn = Some(FunctionContext {
            function: llvm_func,
            labels: HashMap::new(),
            unresolved: HashMap::new(),
        });

        // --- 関数本体 ---
        for stmt in func.body {
            let _value = self.compile_stmt(&stmt, &mut variables);
            // if let Some(ret) = &ret_alloca {
            //     self.builder.build_store((*ret).unwrap(), value);
            // }
        }

        // --- 仮の戻り値 ---
        if return_type_is_void {
            self.builder.build_return(None).unwrap();
        } else {
            // // 仮に i32 を戻り値として返す
            // let zero = self.context.i32_type().const_int(0, false);
            // self.builder.build_return(Some(&zero)).unwrap();
        }
        self.current_fn = None;
    }

    fn compile_struct(&mut self, stc: Struct) {
        if self.struct_types.contains_key(&stc.name) {
            panic!("Struct '{}' already defined", stc.name);
        }
        // ① Opaque StructType を作成（中身は後でセット）
        let struct_type = self.context.opaque_struct_type(&stc.name);

        // ② フィールド型を LLVM 型に変換して格納
        let mut field_types = Vec::new();
        let mut field_info = Vec::new();
        let mut indx: u32 = 0;
        for (field_type_expr, field_name) in &stc.args {
            let field_name_op = match field_name {
                Expr::Ident(s) => Some(s.clone()),
                _ => None,
            };
            let field_name_string =
                field_name_op.expect(&format!("Struct {}:None Field Name", stc.name));
            let llvm_type = self.llvm_type_from_expr(field_type_expr);
            indx += 1;
            field_types.push(llvm_type);
            field_info.push((field_name_string.clone(), llvm_type, indx));
        }

        // ③ 中身を設定
        struct_type.set_body(&field_types, false);
        self.struct_types.insert(stc.name.clone(), struct_type);
        self.struct_fields.insert(stc.name.clone(), field_info);

        //デバッグ時struct表示用
        // self.module
        //     .add_global(struct_type, None, &format!("{}_dummy", stc.name));
    }

    fn compile_stmt(&mut self, stmt: &Stmt, variables: &mut HashMap<String, PointerValue<'ctx>>) {
        match &stmt.expr {
            Expr::IntNumber(_) | Expr::FloatNumber(_) | Expr::Call { .. } | Expr::Ident(_) => {
                // 値の計算は compile_expr に任せる
                self.compile_expr(&stmt.expr, variables);
            }
            Expr::Point(labels) => {
                let label_name = match &labels[0] {
                    Expr::Ident(s) => Some(s.as_str()),
                    _ => None,
                };
                self.gen_point(label_name.expect("point: missing label literal"));
            }
            Expr::Warp { name, args } => match name.as_str() {
                "warpto" => {
                    let label_name = match &args[0] {
                        Expr::Ident(s) => Some(s.as_str()),
                        _ => None,
                    };
                    self.gen_warpto(label_name.expect("point: missing label literal"));
                }
                "warptoif" => {
                    let cond: BasicValueEnum = self.compile_expr(&args[0], variables).unwrap();
                    let cond_i1 = match cond {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("&& requires boolean (i1) values"),
                    };
                    let label_name1 = match &args[1] {
                        Expr::Ident(s) => Some(s.as_str()),
                        _ => None,
                    };
                    let label_name2 = match &args.get(2) {
                        Some(Expr::Ident(s)) => Some(s.as_str()),
                        _ => None,
                    };
                    self.gen_warptoif(
                        cond_i1,
                        label_name1.expect("point: missing label literal"),
                        label_name2,
                    );
                }
                _ => {
                    panic!("warp:not (warpto or warptoif)");
                }
            },
            Expr::Return(vals) => {
                if vals.len() != 1 {
                    panic!("Return must have exactly one value");
                }
                let ret_val = self
                    .compile_expr(vals.into_iter().next().unwrap(), variables)
                    .unwrap();
                self.builder.build_return(Some(&ret_val)).unwrap();
                //None // 既に return しているので上位に値を返す必要なし
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expr(
        &mut self,
        expr: &Expr,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::IntNumber(n) => Some(self.context.i64_type().const_int(*n as u64, false).into()),
            Expr::FloatNumber(n) => Some(self.context.f64_type().const_float(*n).into()),
            Expr::Bool(b) => Some(self.context.bool_type().const_int(*b as u64, false).into()),
            Expr::Char(c) => Some(self.context.i8_type().const_int(*c as u64, false).into()),
            Expr::String(s) => {
                let global_str = self
                    .builder
                    .build_global_string_ptr(s, &format!("str_{}", self.str_counter))
                    .unwrap();
                self.str_counter += 1;
                Some(global_str.as_pointer_value().into())
            }

            Expr::Ident(name) => {
                let alloca = variables.get(name).expect("Undefined variable");
                self.builder.build_load(*alloca, name).unwrap().into()
            }

            Expr::Call { name, args } => match name.as_str() {
                "ptr" | "&_" => {
                    let name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("ptr expects identifier"),
                    };
                    let alloca = get_var_alloca(variables, name);
                    Some(alloca.as_basic_value_enum())
                }
                "val" | "*_" => {
                    let p = self.compile_expr(&args[0], variables).unwrap();
                    let p = p.into_pointer_value();
                    let loaded = self.builder.build_load(p, "deref").unwrap();
                    Some(loaded.as_basic_value_enum())
                }
                "let" | "#=" => {
                    // args: [var_name, initial_value, type_name]
                    let var_name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("let: first arg must be variable name"),
                    };

                    let llvm_type: BasicTypeEnum = self.llvm_type_from_expr(&args[2]);
                    // let type_name = match &args[2] {
                    //     Expr::Ident(s) => s.as_str(),
                    //     _ => panic!("let: third arg must be type name"),
                    // };

                    // // 型を決定
                    // let llvm_type: BasicTypeEnum =
                    //     if let Some(st) = self.struct_types.get(type_name) {
                    //         st.as_basic_type_enum()
                    //     } else {
                    //         match type_name {
                    //             "i32" => self.context.i32_type().into(),
                    //             "i64" => self.context.i64_type().into(),
                    //             "f32" => self.context.f32_type().into(),
                    //             "f64" => self.context.f64_type().into(),
                    //             "bool" => self.context.bool_type().into(),
                    //             "char" => self.context.i8_type().into(),
                    //             "ptr" => self.context.i8_type().ptr_type(Default::default()).into(),
                    //             _ => panic!("Unknown type: {}", type_name),
                    //         }
                    //     };

                    // 初期値がある場合
                    let init_val_exist = match &args[1] {
                        Expr::Ident(s) => {
                            if *s == "_".to_string() {
                                false
                            } else {
                                true
                            }
                        }
                        _ => true,
                    };
                    let init_val = if init_val_exist {
                        self.compile_expr(&args[1], variables)
                    } else {
                        // struct の場合は zeroed で初期化
                        Some(llvm_type.const_zero())
                    };

                    // alloca 作成
                    let alloca = self
                        .builder
                        .build_alloca(llvm_type, var_name)
                        .expect("fail alloca");
                    self.builder.build_store(alloca, init_val.unwrap()).unwrap();
                    variables.insert(var_name.clone(), alloca);

                    init_val
                }

                "=" => {
                    match &args[1] {
                        Expr::ArrayLiteral(elems) => {
                            self.codegen_array_assign(&args[0], elems, variables)
                        }
                        _ => {
                            let value = self.compile_expr(&args[1], variables).unwrap();
                            let alloca = self.get_pointer_expr(&args[0], variables);
                            //let alloca = variables.get(var_name).expect("Undefined variable");
                            self.builder.build_store(alloca, value).unwrap();
                            Some(value)
                        }
                    }
                    // let value = self.compile_expr(&args[1], variables).unwrap();
                    // let alloca = self.get_pointer_expr(&args[0], variables);
                    // //let alloca = variables.get(var_name).expect("Undefined variable");
                    // self.builder.build_store(alloca, value).unwrap();
                    // Some(value)
                }

                "+" | "-" | "*" | "/" => {
                    let lhs_val = self.compile_expr(&args[0], variables)?;
                    let rhs_val = self.compile_expr(&args[1], variables)?;

                    // ===== 型あわせ：左辺の型に右辺を合わせる =====
                    let rhs_casted = match (lhs_val, rhs_val) {
                        // ------- 左右とも整数の場合 -------
                        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                            let lhs_bits = l.get_type().get_bit_width();
                            let rhs_bits = r.get_type().get_bit_width();

                            let r2 = if lhs_bits > rhs_bits {
                                // i32 → i64
                                self.builder
                                    .build_int_s_extend(r, l.get_type(), "int_ext")
                                    .unwrap()
                            } else if lhs_bits < rhs_bits {
                                // i64 → i32
                                self.builder
                                    .build_int_cast(r, l.get_type(), "int_trunc")
                                    .unwrap()
                            } else {
                                r
                            };
                            BasicValueEnum::IntValue(r2)
                        }

                        // ------- 左右とも浮動小数点の場合 -------
                        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                            let lhs_ty = l.get_type();
                            let rhs_ty = r.get_type();

                            let r2 = if lhs_ty != rhs_ty {
                                let lhs_bits = float_bit_width(lhs_ty);
                                let rhs_bits = float_bit_width(rhs_ty);

                                if lhs_bits > rhs_bits {
                                    // f32 → f64
                                    self.builder.build_float_ext(r, lhs_ty, "fext").unwrap()
                                } else {
                                    // f64 → f32
                                    self.builder.build_float_trunc(r, lhs_ty, "ftrunc").unwrap()
                                }
                            } else {
                                r
                            };

                            BasicValueEnum::FloatValue(r2)
                        }

                        // ------- int + float → float（左がfloat） -------
                        (BasicValueEnum::FloatValue(l), BasicValueEnum::IntValue(r)) => {
                            let r2 = self
                                .builder
                                .build_signed_int_to_float(r, l.get_type(), "i2f")
                                .unwrap();
                            BasicValueEnum::FloatValue(r2)
                        }

                        // ------- int + float → int（左がint） -------
                        (BasicValueEnum::IntValue(l), BasicValueEnum::FloatValue(r)) => {
                            let r2 = self
                                .builder
                                .build_float_to_signed_int(r, l.get_type(), "f2i")
                                .unwrap();
                            BasicValueEnum::IntValue(r2)
                        }

                        _ => panic!("Unsupported combination in binary operation"),
                    };

                    // ===== ここから演算 =====
                    let result = match (lhs_val, rhs_casted) {
                        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                            let v = match name.as_str() {
                                "+" => self.builder.build_int_add(l, r, "add").unwrap(),
                                "-" => self.builder.build_int_sub(l, r, "sub").unwrap(),
                                "*" => self.builder.build_int_mul(l, r, "mul").unwrap(),
                                "/" => self.builder.build_int_signed_div(l, r, "div").unwrap(),
                                _ => unreachable!(),
                            };
                            v.as_basic_value_enum()
                        }

                        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                            let v = match name.as_str() {
                                "+" => self.builder.build_float_add(l, r, "fadd").unwrap(),
                                "-" => self.builder.build_float_sub(l, r, "fsub").unwrap(),
                                "*" => self.builder.build_float_mul(l, r, "fmul").unwrap(),
                                "/" => self.builder.build_float_div(l, r, "fdiv").unwrap(),
                                _ => unreachable!(),
                            };
                            v.as_basic_value_enum()
                        }

                        _ => unreachable!(),
                    };

                    Some(result)
                }
                "==" | "!=" | "<=" | ">=" | "<" | ">" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let v = match name.as_str() {
                        "==" => Some(
                            self.build_eq(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        "!=" => Some(
                            self.build_neq(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        "<" => Some(
                            self.build_lt(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        ">" => Some(
                            self.build_gt(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        "<=" => Some(
                            self.build_le(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        ">=" => Some(
                            self.build_ge(compiled_args[0], compiled_args[1])
                                .as_basic_value_enum(),
                        ),
                        _ => None,
                    };
                    v
                }

                "&&" | "||" | "and" | "or" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let lhs_i1 = match compiled_args[0] {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("&& requires boolean (i1) values"),
                    };
                    let rhs_i1 = match compiled_args[1] {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("&& requires boolean (i1) values"),
                    };
                    let v = match name.as_str() {
                        "&&" | "and" => Some(
                            self.builder
                                .build_and(lhs_i1, rhs_i1, "and")
                                .unwrap()
                                .as_basic_value_enum(),
                        ),
                        "||" | "or" => Some(
                            self.builder
                                .build_or(lhs_i1, rhs_i1, "or")
                                .unwrap()
                                .as_basic_value_enum(),
                        ),
                        _ => None,
                    };
                    v
                }
                "!" | "not" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let val_i1 = match compiled_args[0] {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("&& requires boolean (i1) values"),
                    };
                    Some(
                        self.builder
                            .build_not(val_i1, "not_tmp")
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
                "as_i64" | "as_f64" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();

                    match name.as_str() {
                        "as_i64" => Some(
                            self.build_cast_to_i64(compiled_args[0])
                                .as_basic_value_enum(),
                        ),
                        "as_f64" => Some(
                            self.build_cast_to_f64(compiled_args[0])
                                .as_basic_value_enum(),
                        ),
                        _ => None,
                    }
                }
                "parse_i64" | "parse_f64" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    match name.as_str() {
                        "parse_i64" => Some(
                            self.build_cast_to_i64(compiled_args[0])
                                .as_basic_value_enum(),
                        ),
                        "parse_f64" => Some(
                            self.build_cast_to_f64(compiled_args[0])
                                .as_basic_value_enum(),
                        ),
                        _ => None,
                    }
                }
                "if" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    Some(self.build_if_expr(
                        compiled_args[0],
                        compiled_args[1],
                        compiled_args[2],
                        "if",
                    ))
                }
                "println" => {
                    let s_val = self.compile_expr(&args[0], variables).unwrap();
                    let str_ptr = match s_val {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("println expects string pointer"),
                    };
                    self.build_println_from_ptr(str_ptr); // 新しく作る printf 呼び出し
                    None
                }
                "format" => {
                    let fmt_val = self.compile_expr(&args[0], variables).unwrap();
                    // 残り args はフォーマット引数
                    let fmt_ptr = match fmt_val {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("format expects string pointer"),
                    };
                    let arg_vals: Vec<_> = args[1..]
                        .iter()
                        .map(|a| self.compile_expr(a, variables).unwrap())
                        .collect();
                    let res_ptr = self.build_format_from_ptr(fmt_ptr, &arg_vals);
                    Some(res_ptr.into())
                }
                "ptr_add_byte" => {
                    let a = self.compile_expr(&args[0], variables).unwrap();
                    let b = self.compile_expr(&args[1], variables).unwrap();

                    let (ptr_val, idx_int) = match (a, b) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("ptr_add expects (ptr, int) or (int, ptr)"),
                    };
                    let gep = unsafe {
                        self.builder
                            .build_gep(ptr_val, &[idx_int], "ptr_add_byte")
                            .unwrap()
                    };
                    Some(gep.as_basic_value_enum())
                }
                "ptr_add" => {
                    // args: [ptr, idx] または [idx, ptr]
                    let a = self.compile_expr(&args[0], variables).unwrap();
                    let b = self.compile_expr(&args[1], variables).unwrap();

                    let (ptr_val, idx_int) = match (a, b) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("ptr_add expects (ptr, int) or (int, ptr)"),
                    };

                    let gep = unsafe {
                        self.builder
                            .build_gep(ptr_val, &[idx_int], "ptr_add")
                            .unwrap()
                    };
                    Some(gep.as_basic_value_enum())
                }
                "index" => {
                    // args: [ptr, idx] or [idx, ptr] -> load *(ptr + idx)
                    let a = self.compile_expr(&args[0], variables).unwrap();
                    let b = self.compile_expr(&args[1], variables).unwrap();

                    let (ptr_val, idx_int) = match (a, b) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("index expects (ptr,int) or (int,ptr)"),
                    };

                    let gep = unsafe {
                        self.builder
                            .build_gep(ptr_val, &[idx_int], "idx_ptr")
                            .unwrap()
                    };
                    let loaded = self.builder.build_load(gep, "idx_load").unwrap();
                    Some(loaded.as_basic_value_enum())
                }
                "alloc_array" => {
                    // args: [type_name, length_expr]

                    // 1. 要素の型
                    let elem_ty = self.llvm_type_from_expr(&args[0]);

                    // 2. 配列長
                    let len_val = self.compile_expr(&args[1], variables).unwrap();
                    let len_val = match len_val {
                        BasicValueEnum::IntValue(i) => i,
                        _ => panic!("alloc_array: length must be integer"),
                    };

                    // elem_ty が IntType / FloatType / PointerType のいずれかである必要がある
                    let array_ptr = match elem_ty {
                        BasicTypeEnum::IntType(t) => self
                            .builder
                            .build_array_alloca(t, len_val, "array")
                            .unwrap(),
                        BasicTypeEnum::FloatType(t) => self
                            .builder
                            .build_array_alloca(t, len_val, "array")
                            .unwrap(),
                        BasicTypeEnum::PointerType(t) => self
                            .builder
                            .build_array_alloca(t, len_val, "array")
                            .unwrap(),
                        _ => panic!("alloc_array: unsupported type"),
                    };

                    Some(array_ptr.as_basic_value_enum())
                }
                "free" => {
                    self.compile_free(&args[0], variables);
                    None
                }
                "malloc" => {
                    let size_enum = self.compile_expr(&args[0], variables).unwrap();
                    let size = match size_enum {
                        BasicValueEnum::IntValue(i) => Some(i),
                        _ => None,
                    };
                    let ele_type = self.llvm_type_from_expr(&args[1]);
                    Some(
                        self.compile_malloc(size.unwrap(), ele_type)
                            .as_basic_value_enum(),
                    )
                }
                "sizeof" => Some(self.compile_sizeof(&args[0])),
                "_>" => {
                    let value_struct = self.compile_expr(&args[0], variables).unwrap();
                    let memberptr = self.compile_member_access(value_struct, &args[1]);
                    let member_value = self
                        .builder
                        .build_load(memberptr, "getmembervalue")
                        .unwrap();
                    member_value.into()
                }
                "->" => {
                    // let p = self.compile_expr(&args[0], variables).unwrap();
                    // let p = p.into_pointer_value();
                    // let loaded = self.builder.build_load(p, "->deref").unwrap();
                    let value_struct = self.compile_expr(&args[0], variables).unwrap();
                    let memberptr = self.compile_member_access(value_struct, &args[1]);
                    memberptr.as_basic_value_enum().into()
                }
                "scanf" => {
                    let fmt_val = self.compile_expr(&args[0], variables).unwrap();
                    // 残り args はフォーマット引数
                    let fmt_ptr = match fmt_val {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("format expects string pointer"),
                    };
                    let arg_vals: Vec<_> = args[1..]
                        .iter()
                        .map(|a| self.compile_expr(a, variables).unwrap())
                        .collect();
                    self.build_scan_from_ptr(fmt_ptr, &arg_vals);
                    None
                }
                "to_anyptr" => {
                    let ptr = self.compile_expr(&args[0], variables);
                    match ptr {
                        Some(BasicValueEnum::PointerValue(p)) => {
                            Some(self.compile_to_anyptr(p, variables).as_basic_value_enum())
                        }
                        _ => None,
                    }
                }
                "from_anyptr" => {
                    let ptr = self.compile_expr(&args[0], variables);
                    let target_type = self.llvm_type_from_expr(&args[1]);
                    match (ptr, target_type) {
                        (
                            Some(BasicValueEnum::PointerValue(p)),
                            BasicTypeEnum::PointerType(target),
                        ) => Some(self.compile_from_anyptr(p, target).as_basic_value_enum()),
                        _ => None,
                    }
                }
                name => {
                    let func = self
                        .module
                        .get_function(&name)
                        .expect(&format!("Function {} not found", name));
                    let compiled_args: Vec<BasicMetadataValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            BasicMetadataValueEnum::from(val)
                        })
                        .collect();
                    let call_site = self
                        .builder
                        .build_call(func, &compiled_args, "calltmp")
                        .unwrap();
                    if func.get_type().get_return_type().is_some() {
                        Some(call_site.try_as_basic_value().left().unwrap())
                    } else {
                        None
                    }
                } // 他の関数呼び出し
            },

            _ => None,
        }
    }
    fn build_eq(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::EQ, a, b, "eq")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::OEQ, a, b, "eq")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "eq",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for =="),
        }
    }
    fn build_neq(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::NE, a, b, "neq")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::ONE, a, b, "neq")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::NE,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "neq",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    fn build_lt(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::SLT, a, b, "slt")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::OLT, a, b, "slt")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "slt",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    fn build_gt(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::SGT, a, b, "sgt")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::OGT, a, b, "sgt")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::SGT,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "sgt",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    fn build_le(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::SLE, a, b, "sle")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::OLE, a, b, "sle")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::SLE,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "sle",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    fn build_ge(&self, val1: BasicValueEnum<'ctx>, val2: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match (val1, val2) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => self
                .builder
                .build_int_compare(IntPredicate::SGE, a, b, "sge")
                .unwrap(),
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => self
                .builder
                .build_float_compare(FloatPredicate::OGE, a, b, "sge")
                .unwrap(),
            (BasicValueEnum::PointerValue(a), BasicValueEnum::PointerValue(b)) => {
                let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                    let i8ptr_type = self.context.i8_type().ptr_type(AddressSpace::from(0));
                    let fn_type = self
                        .context
                        .i32_type()
                        .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], false);
                    self.module.add_function("strcmp", fn_type, None)
                });

                let result = self
                    .builder
                    .build_call(strcmp_fn, &[a.into(), b.into()], "strcmp_result")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                // strcmp が 0 のとき等しい
                self.builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        result,
                        self.context.i32_type().const_int(0, false),
                        "sge",
                    )
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    fn int_to_i64(&self, v: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64t = self.context.i64_type();
        if v.get_type() == i64t {
            v
        } else {
            // 符号拡張（もし i32 などから来るなら）
            self.builder.build_int_s_extend(v, i64t, "idx_i64").unwrap()
        }
    }
    fn llvm_type_from_expr(&self, expr: &Expr) -> BasicTypeEnum<'ctx> {
        match expr {
            Expr::Ident(name) => {
                if let Some(st) = self.struct_types.get(name) {
                    st.as_basic_type_enum()
                } else {
                    match name.as_str() {
                        "i32" => self.context.i32_type().into(),
                        "i64" => self.context.i64_type().into(),
                        "f32" => self.context.f32_type().into(),
                        "f64" => self.context.f64_type().into(),
                        "bool" => self.context.bool_type().into(),
                        "char" => self.context.i8_type().into(),
                        "T" => self.context.i8_type().into(),
                        "ptr" => self.context.i8_type().ptr_type(Default::default()).into(),
                        _ => panic!("Unknown type: {}", name),
                    }
                }
            }
            Expr::TypeApply { base, args } => match base.as_str() {
                "ptr" => {
                    if args.len() != 1 {
                        panic!("ptr<T> requires exactly one type argument");
                    }
                    let elem_ty = self.llvm_type_from_expr(&args[0]);
                    elem_ty.ptr_type(Default::default()).into()
                }
                _ => panic!("Unknown type constructor: {}", base),
            },
            _ => panic!("Expected identifier type"),
        }
    }
    pub fn build_format_from_ptr(
        &mut self,
        fmt_ptr: PointerValue<'ctx>,
        arg_vals: &[BasicValueEnum<'ctx>],
    ) -> PointerValue<'ctx> {
        // printf/sprintf 関数を取得または作成
        let sprintf_fn = match self.module.get_function("sprintf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.i8_type().ptr_type(Default::default());
                let fn_type = self
                    .context
                    .i32_type()
                    .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], true); // 可変長
                self.module.add_function("sprintf", fn_type, None)
            }
        };

        // 引数を BasicMetadataValueEnum に変換（可変長引数用）
        let meta_args: Vec<BasicMetadataValueEnum> = arg_vals
            .iter()
            .map(|v| BasicMetadataValueEnum::from(*v))
            .collect();

        // フォーマット結果を格納するバッファを作る
        let buf = self
            .builder
            .build_alloca(self.context.i8_type().array_type(128), "fmt_buf")
            .unwrap();

        // i8* 型にキャスト（sprintf に渡すため）
        let buf_ptr = self
            .builder
            .build_bit_cast(
                buf,
                self.context.i8_type().ptr_type(AddressSpace::from(0)),
                "fmt_buf_ptr",
            )
            .unwrap()
            .into_pointer_value();

        // 引数作成（バッファポインタ + フォーマット文字列 + 可変長引数）
        let mut call_args = vec![buf_ptr.into(), fmt_ptr.into()];
        call_args.extend(meta_args.iter().cloned());

        // sprintf 呼び出し
        self.builder
            .build_call(sprintf_fn, &call_args, "sprintf")
            .unwrap();

        // 返り値としてバッファの i8* ポインタを返す
        buf_ptr
    }
    pub fn build_println_from_ptr(&mut self, str_ptr: PointerValue<'ctx>) {
        // printf 関数を取得または作成
        let printf_fn = match self.module.get_function("printf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.i8_type().ptr_type(Default::default());
                let fn_type = self.context.i32_type().fn_type(&[i8ptr_type.into()], true);
                self.module.add_function("printf", fn_type, None)
            }
        };

        // 改行用の新しい文字列リテラルを作る
        let global_str_with_newline = self
            .builder
            .build_global_string_ptr(
                "%s\n", // 文字列を表示して改行
                &format!("println_fmt_{}", self.str_counter),
            )
            .unwrap();
        self.str_counter += 1;

        // printf 呼び出し用引数を作成
        let args: &[BasicMetadataValueEnum] = &[
            global_str_with_newline.as_pointer_value().into(),
            str_ptr.into(),
        ];

        // printf 呼び出し
        self.builder.build_call(printf_fn, args, "printf").unwrap();
    }
    pub fn build_scan_from_ptr(
        &mut self,
        fmt_ptr: PointerValue<'ctx>,
        arg_vals: &[BasicValueEnum<'ctx>],
    ) -> IntValue<'ctx> {
        // scanf 関数を取得 or 宣言
        let scanf_fn = match self.module.get_function("scanf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.i8_type().ptr_type(Default::default());
                let fn_type = self.context.i32_type().fn_type(&[i8ptr_type.into()], true); // 可変長
                self.module.add_function("scanf", fn_type, None)
            }
        };

        // === 引数作成（format + &変数たち） ===
        let mut call_args: Vec<BasicMetadataValueEnum> = vec![fmt_ptr.into()];

        call_args.extend(arg_vals.iter().map(|v| BasicMetadataValueEnum::from(*v)));

        // scanf 呼び出し
        let ret = self
            .builder
            .build_call(scanf_fn, &call_args, "scanf")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        ret.into_int_value()
    }
    fn build_if_expr(
        &mut self,
        cond: BasicValueEnum<'ctx>,
        then_val: BasicValueEnum<'ctx>,
        else_val: BasicValueEnum<'ctx>,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        let cond_i1 = match cond {
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() != 1 {
                    self.builder
                        .build_int_compare(
                            IntPredicate::NE,
                            iv,
                            iv.get_type().const_int(0, false),
                            "cond_bool",
                        )
                        .expect("int_compare failed")
                } else {
                    iv
                }
            }
            _ => panic!("condition must be int/bool"),
        };

        // 型の整合
        let then_type = then_val.get_type();
        let else_val = if then_val.get_type() != else_val.get_type() {
            match (then_val, else_val) {
                (BasicValueEnum::IntValue(_), BasicValueEnum::FloatValue(fv)) => self
                    .builder
                    .build_float_to_signed_int(fv, then_type.into_int_type(), "cast")
                    .expect("cast failed")
                    .as_basic_value_enum(),
                (BasicValueEnum::FloatValue(_), BasicValueEnum::IntValue(iv)) => self
                    .builder
                    .build_signed_int_to_float(iv, then_type.into_float_type(), "cast")
                    .expect("cast failed")
                    .as_basic_value_enum(),
                _ => panic!("unsupported type cast"),
            }
        } else {
            else_val
        };

        self.builder
            .build_select(cond_i1, then_val, else_val, name)
            .unwrap()
    }

    fn build_cast_to_i64(&self, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();

        match value {
            BasicValueEnum::IntValue(v) => {
                // i32 → i64 の可能性もある
                if v.get_type().get_bit_width() < 64 {
                    self.builder
                        .build_int_s_extend(v, i64_type, "i32_to_i64")
                        .unwrap()
                } else {
                    v
                }
            }

            BasicValueEnum::FloatValue(v) => {
                // f64 → i64（切り捨て）
                self.builder
                    .build_float_to_signed_int(v, i64_type, "f64_to_i64")
                    .unwrap()
            }

            BasicValueEnum::PointerValue(ptr) => {
                // これは文字列（i8*）として扱う
                // strtol(i8*, i8**, i32)
                let strtol = self
                    .module
                    .get_function("strtol")
                    .expect("strtol not defined");

                let null_endptr = self
                    .context
                    .i8_type()
                    .ptr_type(Default::default())
                    .const_zero();
                let null_endptr_ptr = self
                    .context
                    .i8_type()
                    .ptr_type(Default::default())
                    .ptr_type(Default::default())
                    .const_zero();

                let endptr = self
                    .builder
                    .build_alloca(
                        self.context.i8_type().ptr_type(Default::default()),
                        "endptr",
                    )
                    .unwrap();
                self.builder.build_store(endptr, null_endptr).unwrap();

                let result = self
                    .builder
                    .build_call(
                        strtol,
                        &[
                            ptr.into(),
                            endptr.into(),
                            self.context.i32_type().const_int(10, false).into(),
                        ],
                        "str_to_i64",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                result.into_int_value()
            }

            _ => panic!("Unsupported type for as_i64"),
        }
    }
    fn build_cast_to_f64(&self, value: BasicValueEnum<'ctx>) -> FloatValue<'ctx> {
        let f64_type = self.context.f64_type();
        let i64_type = self.context.i64_type();

        match value {
            BasicValueEnum::IntValue(v) => {
                // 整数 → 浮動小数
                self.builder
                    .build_signed_int_to_float(v, f64_type, "i64_to_f64")
                    .unwrap()
            }

            BasicValueEnum::FloatValue(v) => v,

            BasicValueEnum::PointerValue(ptr) => {
                // atof(i8*)
                let atof = self.module.get_function("atof").expect("atof not defined");

                let result = self
                    .builder
                    .build_call(atof, &[ptr.into()], "str_to_f64")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                result.into_float_value()
            }

            _ => panic!("Unsupported type for as_f64"),
        }
    }

    pub fn gen_point(&mut self, name: &str) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");

        // ラベル用ブロック作成
        let block = self.context.append_basic_block(current_fn.function, name);

        // ラベル登録
        current_fn.labels.insert(name.to_string(), block);

        // 未解決ジャンプを処理
        if let Some(jumps) = current_fn.unresolved.remove(name) {
            for pending in jumps {
                self.builder.position_at_end(pending.from);
                self.builder.build_unconditional_branch(block);
            }
        }

        // このラベルに builder を移動
        self.builder.position_at_end(block);
    }

    pub fn gen_warpto(&mut self, name: &str) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");
        let current_block = self.builder.get_insert_block().unwrap();

        // ラベルがすでにあるなら即ジャンプ
        if let Some(&target) = current_fn.labels.get(name) {
            self.builder.build_unconditional_branch(target);
            return;
        }

        // まだラベルが定義されていない → unresolved に登録
        current_fn
            .unresolved
            .entry(name.to_string())
            .or_default()
            .push(PendingJump {
                from: current_block,
            });

        // ⚠ 重要：ダミーブロックを作らず、builder を動かさない
        // → 次の point が builder の位置をセットする
    }

    pub fn gen_warptoif(
        &mut self,
        cond: IntValue<'ctx>,
        label_true: &str,
        label_false: Option<&str>,
    ) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");
        let func = current_fn.function;
        let from_block = self.builder.get_insert_block().unwrap();

        // ---- TRUE 側ブロック ----
        let true_block = if let Some(&target) = current_fn.labels.get(label_true) {
            target
        } else {
            // ラベル未定義 → pending ブロックを作成
            let pending = self.context.append_basic_block(func, "pending_true");
            current_fn
                .unresolved
                .entry(label_true.to_string())
                .or_default()
                .push(PendingJump { from: pending });

            pending
        };

        // ---- FALSE 側ブロック ----
        let false_block = match label_false {
            Some(name) => {
                if let Some(&target) = current_fn.labels.get(name) {
                    target
                } else {
                    let pending = self.context.append_basic_block(func, "pending_false");
                    current_fn
                        .unresolved
                        .entry(name.to_string())
                        .or_default()
                        .push(PendingJump { from: pending });
                    pending
                }
            }
            None => {
                // false ラベルなし: else 部分は自然落下ブロック
                self.context.append_basic_block(func, "else_block")
            }
        };

        // ---- 条件分岐を生成 ----
        self.builder
            .build_conditional_branch(cond, true_block, false_block);

        // warptoif はブロックを閉じるだけ
        // builder はどこへも移動しない
        // → 次の point で builder がセットされる
    }

    fn get_pointer_expr(
        &mut self,
        expr: &Expr,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
    ) -> PointerValue<'ctx> {
        match expr {
            Expr::Ident(name) => {
                // 変数 a → その変数の生のポインタ（alloca）
                *variables.get(name).expect("Undefined variable")
            }

            Expr::Call { name, args } if name == "val" => {
                // val(ptr) → ptr の値（= pointer value）
                let p_val = self.compile_expr(&args[0], variables).unwrap();
                p_val.into_pointer_value()
            }

            Expr::Call { name, args } if name == "index" => {
                // allow index(ptr, idx) or index(idx, ptr) (n[p])
                let first = self.compile_expr(&args[0], variables).unwrap();
                let second = self.compile_expr(&args[1], variables).unwrap();

                // decide which is pointer
                if let BasicValueEnum::PointerValue(ptrv) = first {
                    // first is pointer, second must be integer
                    let idx_int = match second {
                        BasicValueEnum::IntValue(iv) => self.int_to_i64(iv),
                        _ => panic!("index: second arg must be integer"),
                    };
                    // GEP
                    let gep =
                        unsafe { self.builder.build_gep(ptrv, &[idx_int], "idx_ptr").unwrap() };
                    gep
                } else if let BasicValueEnum::PointerValue(ptrv) = second {
                    // second is pointer, first is integer (n[p] style)
                    let idx_int = match first {
                        BasicValueEnum::IntValue(iv) => self.int_to_i64(iv),
                        _ => panic!("index: first arg must be integer"),
                    };
                    let gep =
                        unsafe { self.builder.build_gep(ptrv, &[idx_int], "idx_ptr").unwrap() };
                    gep
                } else {
                    panic!("index: one of args must be pointer");
                }
            }

            _ => panic!("Left-hand side must be a pointer or val(ptr)"),
        }
    }

    fn codegen_array_assign(
        &mut self,
        lhs: &Expr,
        elems: &Vec<Expr>,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        // lhs は ptr 型 (i64**)
        // let ptr_to_array = self.get_pointer_expr(lhs, variables);

        // // *lhs をロードして i64* を取得
        // let array_ptr = self
        //     .builder
        //     .build_load(ptr_to_array, "array_ptr")
        //     .unwrap()
        //     .into_pointer_value();
        let array_ptr = self.get_pointer_expr_only_val(lhs, variables);

        for (i, elem) in elems.iter().enumerate() {
            let index_val = self.context.i64_type().const_int(i as u64, false);

            // 正しい GEP (i64* にオフセット)
            let gep = unsafe {
                self.builder
                    .build_gep(
                        array_ptr,    // i64*
                        &[index_val], // index
                        "array_idx",
                    )
                    .unwrap()
            };

            // 値生成
            let val = self.compile_expr(elem, variables).unwrap();

            // store
            self.builder.build_store(gep, val).unwrap();
        }

        Some(array_ptr.as_basic_value_enum())
    }

    fn compile_malloc(
        &self,
        size: IntValue<'ctx>,
        element_type: BasicTypeEnum<'ctx>, // i64_type, i8_type, f64_type など
    ) -> PointerValue<'ctx> {
        let malloc_fn = self
            .module
            .get_function("malloc")
            .expect("malloc not defined");
        let i8ptr = self
            .builder
            .build_call(malloc_fn, &[size.into()], "malloc_call")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        // ★ malloc の戻り値 (i8*) を必要な型 T* に変換する
        let typed_ptr = self
            .builder
            .build_bit_cast(
                i8ptr,
                element_type.ptr_type(AddressSpace::default()),
                "cast_ptr",
            )
            .unwrap()
            .into_pointer_value();

        typed_ptr
    }

    fn compile_member_access(
        &mut self,
        struct_value_enum: BasicValueEnum<'ctx>,
        field_name_expr: &Expr,
    ) -> PointerValue<'ctx> {
        let field_name = match field_name_expr {
            Expr::Ident(s) => s.clone(),
            _ => "".to_string(),
        };
        let struct_value = match struct_value_enum {
            BasicValueEnum::PointerValue(ptr_val) => ptr_val,
            _ => {
                // エラー処理: ポインタ値ではない
                panic!("メンバーアクセスはポインタ値に対して行う必要があります。");
            }
        };

        let struct_anytype = struct_value.get_type().get_element_type();
        let struct_type: StructType<'ctx> = match struct_anytype {
            AnyTypeEnum::StructType(struct_t) => struct_t,
            _ => {
                // エラー処理: ポインタが構造体型を指していない
                panic!("ポインタが構造体型を指していません。");
            }
        };
        let key = self
            .struct_types
            .iter()
            .find(|(_, v)| **v == struct_type)
            .map(|(k, _)| k.clone());
        let struct_type_name = key.unwrap();
        let indx = self.struct_fields[&struct_type_name]
            .iter()
            .find(|v| v.0 == field_name.as_str())
            .map(|v| v.2)
            .unwrap();
        let return_ptr = self
            .builder
            .build_struct_gep(struct_value, indx, "access")
            .unwrap();
        return_ptr
    }

    fn compile_sizeof(&self, ty_expr: &Expr) -> BasicValueEnum<'ctx> {
        let llvm_ty = self.llvm_type_from_expr(ty_expr);

        let size = llvm_ty.size_of().expect("LLVM failed to compute size_of");

        size.as_basic_value_enum()
    }
    fn compile_to_anyptr(
        &mut self,
        ptr: PointerValue<'ctx>,
        _variables: &HashMap<String, PointerValue<'ctx>>,
    ) -> PointerValue<'ctx> {
        let i8_ptr = self.context.i8_type().ptr_type(AddressSpace::from(0));
        self.builder
            .build_bit_cast(ptr, i8_ptr, "to_anyptr")
            .unwrap()
            .into_pointer_value()
    }
    fn compile_from_anyptr(
        &mut self,
        any: PointerValue<'ctx>,
        target: PointerType<'ctx>,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_bit_cast(any, target, "from_anyptr")
            .unwrap()
            .into_pointer_value()
    }
    fn compile_free(
        &mut self,
        ptr_expr: &Expr,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
    ) {
        // ポインタの値を取得（例: i64*）
        // let ptr = self.get_pointer_expr(ptr_expr, variables);

        // let free_ptr = match ptr_expr{
        //     Expr::Ident(_)=> self
        //     .builder
        //     .build_load(ptr, "free_ptr")
        //     .unwrap()
        //     .into_pointer_value(),
        //     _ => ptr,
        // };
        let free_ptr = self.get_pointer_expr_only_val(ptr_expr, variables);
        // i8* へ bitcast
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let casted = self
            .builder
            .build_bit_cast(free_ptr, i8_ptr_type, "free_cast")
            .unwrap()
            .into_pointer_value();
        let free_fn = self.module.get_function("free").expect("free not defined");
        // free を呼び出す
        self.builder
            .build_call(free_fn, &[casted.into()], "free_call")
            .unwrap();
    }
    fn get_pointer_expr_only_val(
        &mut self,
        ptr_expr: &Expr,
        variables: &mut HashMap<String, PointerValue<'ctx>>,
    ) -> PointerValue<'ctx> {
        let ptr = self.get_pointer_expr(ptr_expr, variables);

        let ptr_val = match ptr_expr {
            Expr::Ident(_) => self
                .builder
                .build_load(ptr, "get_ptr")
                .unwrap()
                .into_pointer_value(),
            _ => ptr,
        };
        ptr_val
    }
    fn init_external_functions(&self) {
        let context = self.context;

        // strtol(i8*, i8**, i32)
        self.module.add_function(
            "strtol",
            context.i64_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context
                        .i8_type()
                        .ptr_type(Default::default())
                        .ptr_type(Default::default())
                        .into(),
                    context.i32_type().into(),
                ],
                false,
            ),
            None,
        );

        // atof(i8*)
        self.module.add_function(
            "atof",
            context.f64_type().fn_type(
                &[context.i8_type().ptr_type(Default::default()).into()],
                false,
            ),
            None,
        );

        // printf(i8*, ...)
        self.module.add_function(
            "printf",
            context.i32_type().fn_type(
                &[context.i8_type().ptr_type(Default::default()).into()],
                true,
            ),
            None,
        );

        // sprintf(i8*, i8*, ...)
        self.module.add_function(
            "sprintf",
            context.i32_type().fn_type(
                &[
                    context.i8_type().ptr_type(Default::default()).into(),
                    context.i8_type().ptr_type(Default::default()).into(),
                ],
                true,
            ),
            None,
        );

        //malloc(size,type)
        let i8_ptr_type = context.i8_type().ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();
        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
        let malloc_fn = self.module.add_function("malloc", malloc_type, None);

        //free(ptr)
        let void_type = self.context.void_type();
        let i8_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let fn_type = void_type.fn_type(&[i8_ptr.into()], false);
        self.module.add_function("free", fn_type, None);

        //scanf
        let i8ptr_type = context.i8_type().ptr_type(AddressSpace::default());
        let scanf_type = context.i32_type().fn_type(&[i8ptr_type.into()], true);
        self.module.add_function("scanf", scanf_type, None);
    }
}
fn float_bit_width(ft: FloatType) -> u32 {
    let printed = ft.print_to_string(); // 所有権を保持する
    let name = printed.to_str().unwrap(); // 借用してもOK
    //let name = ft.print_to_string().to_string_lossy();

    if name == "float" {
        32
    } else if name == "double" {
        64
    } else if name == "half" {
        16
    } else if name == "fp128" {
        128
    } else {
        panic!("Unsupported float type: {}", name);
    }
}
fn combine_toplevel<'ctx>(module: &Module<'ctx>, builder: &Builder<'ctx>) {
    // 1. モジュール内の <toplevel_child> 関数を収集
    let mut toplevel_funcs: Vec<FunctionValue> = vec![];

    for f in module.get_functions() {
        let name = f.get_name().to_str().unwrap();
        if name.starts_with("toplevel_child") {
            toplevel_funcs.push(f);
        }
    }

    if toplevel_funcs.is_empty() {
        return; // まとめるものがない場合
    }
    // 2. main の関数型を作る: i32 main(i32, i8**)
    let i32_type = module.get_context().i32_type();
    let i8_ptr_type = module
        .get_context()
        .i8_type()
        .ptr_type(AddressSpace::from(0));
    let main_fn_type = i32_type.fn_type(
        &[
            i32_type.into(),
            i8_ptr_type.ptr_type(AddressSpace::from(0)).into(),
        ],
        false,
    );
    // 3. main 関数を追加
    let main_fn = module.add_function("main", main_fn_type, None);
    let argc = main_fn.get_nth_param(0).unwrap();
    argc.set_name("argc");
    let argv = main_fn.get_nth_param(1).unwrap();
    argv.set_name("argv");
    // 4. エントリーブロック作成
    let entry_bb = module.get_context().append_basic_block(main_fn, "entry");
    builder.position_at_end(entry_bb);

    // 5. toplevel_child を順に call
    for f in toplevel_funcs.iter() {
        builder
            .build_call(*f, &[], &format!("call_{}", f.get_name().to_str().unwrap()))
            .unwrap();
    }

    // 6. i32 0 を返す
    builder
        .build_return(Some(&i32_type.const_int(0, false)))
        .unwrap();
}

fn get_var_alloca<'ctx>(
    variables: &HashMap<String, PointerValue<'ctx>>,
    name: &str,
) -> PointerValue<'ctx> {
    *variables
        .get(name)
        .unwrap_or_else(|| panic!("Undefined variable: {}", name))
}
