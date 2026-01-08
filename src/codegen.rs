use colored::*;
use core::panic;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::InlineAsmDialect;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::CodeModel;
use inkwell::targets::InitializationConfig;
use inkwell::targets::RelocMode;
use inkwell::targets::Target;
use inkwell::targets::TargetData;
use inkwell::targets::TargetMachine;
use inkwell::targets::TargetTriple;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FloatType;
use inkwell::types::*;
use inkwell::values::*;
use std::collections::HashMap;

use crate::parser::*;

/// Reprsents a jump that needs to be resolved (e.g., target block is not yet created).
struct PendingJump<'ctx> {
    from: BasicBlock<'ctx>,
}
/// Stores the LLVM pointer and the original AST type expression for a variable.
#[derive(Clone)]
struct VariablesPointerAndTypes<'ctx> {
    ptr: PointerValue<'ctx>,
    typeexpr: Expr,
}
/// Holds context information for the function currently being compiled.
struct FunctionContext<'ctx> {
    function: FunctionValue<'ctx>,
    labels: HashMap<String, BasicBlock<'ctx>>, // mapping of label names to their basic blocks (already existing)
    unresolved: HashMap<String, Vec<PendingJump<'ctx>>>, // jumps to labels that haven't been defined yet
    return_ty: Expr,
    fn_name: String,
}
/// Manages memory ownership scopes to track potential leaks.
#[derive(Clone)]
pub struct ScopeOwner {
    pos: usize,
    owners: Vec<HashMap<String, bool>>,
}
impl ScopeOwner {
    fn new() -> Self {
        Self {
            pos: 0,
            owners: vec![HashMap::new()],
        }
    }
    fn back(&mut self) {
        self.pos -= if self.pos != 0 { 1 } else { 0 };
    }
    fn next(&mut self) {
        self.pos += 1;
        if self.owners.len() <= self.pos {
            self.owners.push(HashMap::new());
            if self.owners.len() <= self.pos {
                panic!("ScopeOwner fail next");
            }
        } else {
            self.owners[self.pos] = HashMap::new();
        }
    }
    fn reset_current(&mut self) {
        self.owners[self.pos] = HashMap::new();
    }
    fn set_true(&mut self, name: String) {
        self.owners[self.pos].insert(name, true);
    }
    fn _set_false(&mut self, name: String) {
        self.owners[self.pos].insert(name, false);
    }
    fn show_current(&mut self) -> HashMap<String, bool> {
        self.owners[self.pos].clone()
    }
}
/// Information about a global variable.
struct GlobalVar<'ctx> {
    ptr: GlobalValue<'ctx>,
    ty_ast: Expr,
}
/// The core Code Generator structure that maintains LLVM state and WapL context.
pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub struct_types: HashMap<String, StructType<'ctx>>,
    pub struct_fields: HashMap<String, Vec<(String, BasicTypeEnum<'ctx>, u32, Expr)>>,
    str_counter: usize, // unique counter for global string names
    current_fn: Option<FunctionContext<'ctx>>,
    pub function_types: HashMap<String, (Expr, bool)>,
    pub current_owners: HashMap<String, bool>,
    pub scope_owners: ScopeOwner,
    global_variables: HashMap<String, GlobalVar<'ctx>>,
    pub bitsize: String,
}

impl<'ctx> Codegen<'ctx> {
    /// Initializes a new Codegen instance.
    ///
    /// # Arguments
    /// * `context` - The LLVM context.
    /// * `name` - The name of the module.
    /// * `bitsize` - The size of numeric types in bits ("32" or "64").
    /// * `wasm` - Whether to target WebAssembly.
    /// * `browser` - Whether to target a browser environment (affects WASM target triple).
    pub fn new(
        context: &'ctx Context,
        name: &str,
        bitsize: String,
        wasm: bool,
        browser: bool,
    ) -> Self {
        Target::initialize_all(&InitializationConfig::default());
        let module = context.create_module(name);
        let builder = context.create_builder();

        let this = Self {
            context,
            module,
            builder,
            struct_types: HashMap::new(),
            struct_fields: HashMap::new(),
            str_counter: 0,
            current_fn: None,
            function_types: HashMap::new(),
            current_owners: HashMap::new(),
            scope_owners: ScopeOwner::new(),
            global_variables: HashMap::new(),
            bitsize: bitsize.clone(),
        };
        if bitsize.as_str() == "32" && wasm && !browser {
            this.module.set_triple(&TargetTriple::create("wasm32-wasi"));
            this.module.set_data_layout(
                &TargetData::create("e-m:e-p:32:32-i64:64-n32:64-S128").get_data_layout(),
            );
        } else if bitsize.as_str() == "32" && wasm && browser {
            this.module
                .set_triple(&TargetTriple::create("wasm32-unknown-unknown"));
            this.module.set_data_layout(
                &TargetData::create("e-m:e-p:32:32-i64:64-n32:64-S128").get_data_layout(),
            );
        } else {
            // Native (e.g., x86_64-linux-gnu)
            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();

            let tm = target
                .create_target_machine(
                    &triple,
                    "generic",
                    "",
                    OptimizationLevel::Default,
                    RelocMode::Default,
                    CodeModel::Default,
                )
                .unwrap();

            this.module.set_triple(&triple);
            this.module
                .set_data_layout(&tm.get_target_data().get_data_layout());
        }
        if !browser {
            this.init_external_functions(); // declare C functions  
        }

        this
    }

    /// Compiles the entire program AST.
    pub fn compile_program(&mut self, program: Program) {
        if program.has_main {
            self.compile_declare(Declare {
                name: "_TOPLEVEL_".to_string(),
                return_type: Expr::Ident("i32".to_string()),
                args: vec![],
                is_vararg: false,
            });
        }

        for func in program.functions {
            match func {
                TopLevel::Function(f) => {
                    self.compile_function(f);
                }
                TopLevel::Struct(s) => {
                    self.compile_struct(s);
                }
                TopLevel::Declare(d) => {
                    self.compile_declare(d);
                }
                TopLevel::Export(e) => {
                    self.compile_export(e);
                }
            }
        }
        combine_toplevel(&self.module, &self.builder, program.has_main);
    }
    /// Compiles an exported function, making it visible to external modules (e.g., WASM exports).
    fn compile_export(&mut self, export: Export) {
        let name = export.name;
        let func = self
            .module
            .get_function(&name)
            .expect(&format!("export:Function {} not found", name));
        func.set_linkage(Linkage::External);
        let attr = self
            .context
            .create_string_attribute("wasm-export-name", &name);

        func.add_attribute(AttributeLoc::Function, attr);
    }
    /// Compiles a declared function (a function whose signature was previously defined).
    fn compile_declared_function(&mut self, name: String, func: Function) {
        let return_type_is_void = matches!(func.return_type, Expr::Ident(ref s) if s == "void");

        let return_type_enum = if return_type_is_void {
            None
        } else {
            Some(self.llvm_type_from_expr(&func.return_type))
        };

        // --- argument types ---
        let arg_types: Vec<BasicTypeEnum> = func
            .args
            .iter()
            .map(|(ty, _)| self.llvm_type_from_expr(ty))
            .collect();

        // --- convert to Metadata type ---
        let arg_types_meta: Vec<BasicMetadataTypeEnum> =
            arg_types.iter().map(|t| (*t).into()).collect();

        // --- generate LLVM function signature ---
        let _fn_type = if return_type_is_void {
            self.context.void_type().fn_type(&arg_types_meta, false)
        } else {
            return_type_enum.unwrap().fn_type(&arg_types_meta, false)
        };
        let llvm_func = self
            .module
            .get_function(&name)
            .expect(&format!("Function {} not found", name));
        let entry = self.context.append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(entry);
        // --- allocate and initialize arguments ---
        self.current_owners = HashMap::new();
        self.scope_owners = ScopeOwner::new();
        let mut variables: HashMap<String, VariablesPointerAndTypes<'ctx>> = HashMap::new();
        for (i, (ty, arg_expr)) in func.args.iter().enumerate() {
            let param = llvm_func.get_nth_param(i as u32).unwrap();

            // get arg names (Expr::Ident(name))
            let arg_name = match arg_expr {
                Expr::Ident(name) => name.as_str(),
                _ => panic!("Function argument name must be identifier"),
            };
            param.set_name(arg_name);

            // alloca anyway
            let alloca = self
                .builder
                .build_alloca(param.get_type(), arg_name)
                .expect("alloca failed");
            self.builder.build_store(alloca, param).unwrap();
            variables.insert(
                arg_name.to_string(),
                VariablesPointerAndTypes {
                    ptr: alloca,
                    typeexpr: ty.clone(),
                },
            );
            match ty {
                Expr::TypeApply { base, args: _args } if base == "*" => {
                    self.current_owners.insert(arg_name.to_string(), true);
                    self.scope_owners.set_true(arg_name.to_string());
                }
                _ => {}
            }
        }
        //alloca return value
        let _ret_alloca = if !return_type_is_void {
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
            return_ty: func.return_type,
            fn_name: name.clone(),
        });

        // --- function body ---
        for stmt in func.body {
            let _value = self.compile_stmt(&stmt, &mut variables);
        }

        // --- temporary return ---
        if return_type_is_void {
            self.builder.build_return(None).unwrap();
        } else {
            // TODO: Ensure a return statement exists for non-void functions
        }
        //Exit from the current function
        self.current_fn = None;
    }

    /// Compiles a new function definition.
    fn compile_function(&mut self, func: Function) {
        if self.function_types.contains_key(&func.name) && self.function_types[&func.name].1 {
            panic!("function '{}' already defined", func.name);
        } else if self.function_types.contains_key(&func.name) && !self.function_types[&func.name].1
        {
            self.compile_declared_function(func.name.clone(), func.clone());
            return;
        }
        // --- type of return value ---
        let return_type_is_void = matches!(func.return_type, Expr::Ident(ref s) if s == "void");

        let return_type_enum = if return_type_is_void {
            None
        } else {
            Some(self.llvm_type_from_expr(&func.return_type))
        };

        // --- type of arguments ---
        let arg_types: Vec<BasicTypeEnum> = func
            .args
            .iter()
            .map(|(ty, _)| self.llvm_type_from_expr(ty))
            .collect();

        // --- convert to Metadata type ---
        let arg_types_meta: Vec<BasicMetadataTypeEnum> =
            arg_types.iter().map(|t| (*t).into()).collect();

        // --- generate LLVM function signature ---
        let fn_type = if return_type_is_void {
            self.context.void_type().fn_type(&arg_types_meta, false)
        } else {
            return_type_enum.unwrap().fn_type(&arg_types_meta, false)
        };

        // --- add function ---
        let llvm_func = self.module.add_function(&func.name, fn_type, None);
        self.function_types
            .insert(func.name.clone(), (func.return_type.clone(), true));
        let entry = self.context.append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(entry);

        // --- allocate and initialize arguments ---
        self.current_owners = HashMap::new();
        self.scope_owners = ScopeOwner::new();
        let mut variables: HashMap<String, VariablesPointerAndTypes<'ctx>> = HashMap::new();
        for (i, (ty, arg_expr)) in func.args.iter().enumerate() {
            let param = llvm_func.get_nth_param(i as u32).unwrap();

            // get arg names (Expr::Ident(name))
            let arg_name = match arg_expr {
                Expr::Ident(name) => name.as_str(),
                _ => panic!("Function argument name must be identifier"),
            };
            param.set_name(arg_name);

            // alloca anyway
            let alloca = self
                .builder
                .build_alloca(param.get_type(), arg_name)
                .expect("alloca failed");
            self.builder.build_store(alloca, param).unwrap();
            variables.insert(
                arg_name.to_string(),
                VariablesPointerAndTypes {
                    ptr: alloca,
                    typeexpr: ty.clone(),
                },
            );
            match ty {
                Expr::TypeApply { base, args: _args } if base == "*" => {
                    self.current_owners.insert(arg_name.to_string(), true);
                    self.scope_owners.set_true(arg_name.to_string());
                }
                _ => {}
            }
        }
        //alloca return value
        let _ret_alloca = if !return_type_is_void {
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
            return_ty: func.return_type.clone(),
            fn_name: func.name.clone(),
        });

        // --- function body ---
        for stmt in func.body {
            let _value = self.compile_stmt(&stmt, &mut variables);
        }

        // --- temporary return ---
        if return_type_is_void {
            self.builder.build_return(None).unwrap();
        } else {
            // TODO: Ensure a return statement exists for non-void functions
        }
        //Exit from the current function
        self.current_fn = None;
    }

    /// Compiles a struct definition.
    fn compile_struct(&mut self, stc: Struct) {
        if self.struct_types.contains_key(&stc.name) {
            panic!("Struct '{}' already defined", stc.name);
        }
        // Create an opaque LLVM StructType; the body will be set later
        let struct_type = self.context.opaque_struct_type(&stc.name);

        // Convert each field type to LLVM type and collect field info (name, type, index)
        let mut field_types = Vec::new();
        let mut field_info = Vec::new();
        let mut indx: u32 = 0; //field index in struct
        for (field_type_expr, field_name) in &stc.args {
            let field_name_op = match field_name {
                Expr::Ident(s) => Some(s.clone()),
                _ => None,
            };
            // Extract field name as string; panic if field is not an identifier
            let field_name_string =
                field_name_op.expect(&format!("Struct {}:None Field Name", stc.name));
            let llvm_type = self.llvm_type_from_expr(field_type_expr);
            field_types.push(llvm_type);
            field_info.push((
                field_name_string.clone(),
                llvm_type,
                indx,
                field_type_expr.clone(),
            ));
            indx += 1;
        }

        // set struct body in LLVM
        struct_type.set_body(&field_types, false);
        self.struct_types.insert(stc.name.clone(), struct_type);
        self.struct_fields.insert(stc.name.clone(), field_info);

        //For displaying struct during debugging
        // self.module
        //     .add_global(struct_type, None, &format!("{}_dummy", stc.name));
    }

    /// Compiles a statement.
    ///
    /// # Returns
    /// * `bool` - True if execution should continue, False if a return was encountered.
    fn compile_stmt(
        &mut self,
        stmt: &Stmt,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> bool {
        match &stmt.expr {
            Expr::IntNumber(_) | Expr::FloatNumber(_) | Expr::Call { .. } | Expr::Ident(_) => {
                // compile_expr
                self.compile_expr(&stmt.expr, variables);
                true
            }
            Expr::Point(labels) => {
                let label_name = match &labels[0] {
                    Expr::Ident(s) => Some(s.as_str()),
                    _ => None,
                };
                // Define a label (point) in the code
                self.gen_point(label_name.expect("point: missing label literal"));
                true
            }
            Expr::Warp { name, args } => match name.as_str() {
                "warpto" => {
                    // get label name (e.g., in `warpto label_name`)
                    let label_name = match &args[0] {
                        Expr::Ident(s) => Some(s.as_str()),
                        _ => None,
                    };
                    self.gen_warpto(label_name.expect("point: missing label literal"));
                    true
                }
                "warptoif" => {
                    // compile condition value
                    let cond: BasicValueEnum = self.compile_expr(&args[0], variables).unwrap().0;
                    let cond_i1 = match cond {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("warptoif condition requires boolean (i1) values"),
                    };
                    //get label name (point NAME <- this)
                    // label_name1 = destination if condition true
                    // label_name2 = destination if condition false (optional)
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
                    true
                }
                _ => {
                    panic!("warp:not (warpto or warptoif)");
                }
            },
            Expr::Return(vals) => {
                // Handle 'return' statement
                if type_match(
                    &(self
                        .current_fn
                        .as_ref()
                        .expect("return: out of function")
                        .return_ty
                        .clone()),
                    &Expr::Ident("void".to_string()),
                ) {
                    self.builder.build_return(None).unwrap();
                    return false;
                }
                if vals.len() != 1 {
                    panic!("Return must have exactly one value");
                }
                //compile return value
                let (ret_val, ret_ty, _) = self
                    .compile_expr(vals.into_iter().next().unwrap(), variables)
                    .unwrap(); // unwrap is safe because we already checked vals.len() == 1
                self.builder.build_return(Some(&ret_val)).unwrap();
                if !type_match(
                    &ret_ty,
                    &(self
                        .current_fn
                        .as_ref()
                        .expect("return: out of function")
                        .return_ty
                        .clone()),
                ) {
                    println!(
                        "{}:function {} expected {:?} found{:?}",
                        "Error".red(),
                        self.current_fn
                            .as_ref()
                            .expect("return: out of function")
                            .fn_name
                            .clone(),
                        self.current_fn
                            .as_ref()
                            .expect("return: out of function")
                            .return_ty
                            .clone(),
                        ret_ty
                    )
                }
                // check memory leaks (ownership)
                for i in self.scope_owners.show_current() {
                    // if some memory hasn't been released, print an error message
                    if let Some(b) = self.current_owners.get(&i.0)
                        && *b
                    {
                        println!(
                            "{}:you need to free or drop pointer {}! at function {}",
                            "Error".red().bold(),
                            i.0,
                            self.current_fn
                                .as_ref()
                                .expect("return: out of function")
                                .fn_name
                                .clone(),
                        );
                    }
                }
                self.scope_owners.reset_current();
                false
            }
            Expr::Loopif { name, cond, body } => {
                if cond.len() != 1 {
                    panic!("Loopif:{} conditions must have exactly one value", name);
                }

                self.compile_loopif(name, &cond[0], body, variables);
                false
            }
            Expr::If {
                branches,
                else_block,
            } => self.compile_if(branches.clone(), else_block.clone(), variables),
            _ => unimplemented!(),
        }
    }

    /// Compiles an expression.
    ///
    /// # Returns
    /// * `Option<(BasicValueEnum, Expr, Option<VariablesPointerAndTypes>)>` -
    ///   The compiled value, its type expression, and optionally its variable pointer information.
    fn compile_expr(
        &mut self,
        expr: &Expr,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> Option<(
        BasicValueEnum<'ctx>,
        Expr,
        Option<VariablesPointerAndTypes<'ctx>>,
    )> {
        match expr {
            // Literals
            Expr::IntNumber(n) => Some((
                self.context.i64_type().const_int(*n as u64, false).into(),
                Expr::Ident("i64".to_string()),
                None,
            )),
            Expr::FloatNumber(n) => Some((
                self.context.f64_type().const_float(*n).into(),
                Expr::Ident("f64".to_string()),
                None,
            )),
            Expr::IntSNumber(n) => Some((
                self.context.i32_type().const_int(*n as u64, false).into(),
                Expr::Ident("i32".to_string()),
                None,
            )),
            Expr::IsizeNumber(n) => Some((
                self.llvm_type_from_expr(&Expr::Ident("isize".to_string()))
                    .into_int_type()
                    .const_int(*n as u64, false)
                    .into(),
                Expr::Ident("isize".to_string()),
                None,
            )),
            Expr::FloatSNumber(n) => Some((
                self.context.f32_type().const_float((*n).into()).into(),
                Expr::Ident("f32".to_string()),
                None,
            )),
            Expr::Bool(b) => Some((
                self.context.bool_type().const_int(*b as u64, false).into(),
                Expr::Ident("bool".to_string()),
                None,
            )),
            Expr::Char(c) => Some((
                self.context.i8_type().const_int(*c as u64, false).into(),
                Expr::Ident("char".to_string()),
                None,
            )),
            Expr::String(s) => {
                // create a unique named global string
                let global_str = self
                    .builder
                    .build_global_string_ptr(s, &format!("str_{}", self.str_counter))
                    .unwrap();
                self.str_counter += 1;
                Some((
                    global_str.as_pointer_value().into(),
                    Expr::TypeApply {
                        base: "ptr".to_string(), // type is ptr:char
                        args: vec![Expr::Ident("char".to_string())],
                    },
                    None,
                ))
            }

            // Variables
            Expr::Ident(name) => {
                // get pointer and variable type
                let alloca = variables
                    .get(name)
                    .expect(&format!("Undefined variable {}", name)); // safe because variable must exist
                match &alloca.typeexpr {
                    // borrow check
                    Expr::TypeApply { base, args } if base == "*" => {
                        // check ownership: if pointer has been moved, reading it is prohibited
                        if !self.current_owners.get(name).expect(&format!(
                            "{} type is *:{:?} but failed to find in ownerships ",
                            name, args[0]
                        )) {
                            println!(
                                "{}:\"{}\" already moved. it is prohibited to read moved pointer: at function {}",
                                "Error".red().bold(),
                                name,
                                self.current_fn
                                    .as_ref()
                                    .expect("return: out of function")
                                    .fn_name
                                    .clone(),
                            )
                        }
                    }
                    _ => {}
                }
                Some((
                    self.builder
                        .build_load(self.llvm_type_from_expr(&alloca.typeexpr), alloca.ptr, name)
                        .unwrap()
                        .into(),
                    alloca.typeexpr.clone(),
                    Some(alloca.clone()),
                ))
            }

            Expr::Call { name, args } => match name.as_str() {
                // return reference
                "ptr" | "&_" => {
                    // if arg is variable, get its pointer directly
                    // otherwise, compile the expression and ensure it's an lvalue
                    let name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => {
                            // unwrap because "ptr" or "&_" require an expression with a pointer
                            let (exp, ty, p) = self.compile_expr(&args[0], variables).unwrap();
                            let ptr = self.ensure_lvalue(exp, &ty, p).as_basic_value_enum();
                            return Some((
                                ptr.clone(),
                                Expr::TypeApply {
                                    base: "ptr".to_string(),
                                    args: vec![ty],
                                },
                                None,
                            ));
                        }
                    };
                    let alloca = get_var_alloca(variables, name);
                    Some((
                        alloca.as_basic_value_enum(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![
                                variables
                                    .get(name)
                                    .expect(&format!("Undefined variable {}", name))
                                    .typeexpr
                                    .clone(),
                            ],
                        },
                        None,
                    ))
                }
                // dereference: val(pointer, optional_type) or *_(pointer, optional_type)
                "val" | "*_" => {
                    // p = (pointer value, type, ...)
                    // safe: "val" / "*_" always expects an expression that evaluates to a pointer
                    let p = self.compile_expr(&args[0], variables).unwrap();
                    let ptr = p.0.into_pointer_value();
                    let mut load_type = &expr_deref(&p.1); // type the pointer points to
                    let ty = args.get(1);
                    // Determine the type to load: default is pointer's base type, override if second argument is provided
                    load_type = match ty {
                        Some(t) => t,
                        None => load_type,
                    };
                    let loaded = self
                        .builder
                        .build_load(self.llvm_type_from_expr(load_type), ptr, "deref")
                        .unwrap();
                    Some((
                        loaded.as_basic_value_enum(),
                        load_type.clone(),
                        Some(VariablesPointerAndTypes {
                            ptr,
                            typeexpr: load_type.clone(),
                        }),
                    ))
                }
                "#=_global" => {
                    // Global variable declaration: args: [var_name, initial_value, type_name]
                    let var_name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("let: first arg must be variable name"),
                    };

                    let llvm_type: BasicTypeEnum = self.llvm_type_from_expr(&args[2]);

                    // If there is an initial value
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
                    // Check if an initial value is provided; "_" means no initial value
                    // If no initial value, zero-initialize (works for numeric types and structs)
                    let init_val = if init_val_exist {
                        self.compile_expr(&args[1], variables)
                    } else {
                        // For structs, initialize with zeroed
                        Some((
                            llvm_type.const_zero(),
                            Expr::Ident("void".to_string()),
                            None,
                        ))
                    };
                    let gv =
                        self.module
                            .add_global(llvm_type, Some(AddressSpace::default()), var_name);
                    gv.set_initializer(&init_val.clone().unwrap().0);
                    self.global_variables.insert(
                        var_name.clone(),
                        GlobalVar {
                            ptr: gv,
                            ty_ast: args[2].clone(),
                        },
                    );
                    None
                }
                "#=_extern_global" => {
                    // args: [var_name,type_name]
                    let var_name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("extern_global: first arg must be variable name"),
                    };

                    let llvm_type: BasicTypeEnum = self.llvm_type_from_expr(&args[1]);

                    let gv = self.module.add_global(llvm_type, None, var_name);
                    gv.set_externally_initialized(true);
                    gv.set_linkage(inkwell::module::Linkage::External);
                    self.global_variables.insert(
                        var_name.clone(),
                        GlobalVar {
                            ptr: gv,
                            ty_ast: args[1].clone(),
                        },
                    );
                    None
                }
                "#=_export_global" => {
                    // Exported global variable (visible to external modules like WASM)
                    // args: [var_name, type_name]
                    let var_name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("export_global: first arg must be variable name"),
                    };

                    let llvm_type: BasicTypeEnum = self.llvm_type_from_expr(&args[1]);

                    let gv = self.module.add_global(llvm_type, None, var_name);
                    // gv.set_externally_initialized(true);
                    gv.set_linkage(inkwell::module::Linkage::External);
                    self.global_variables.insert(
                        var_name.clone(),
                        GlobalVar {
                            ptr: gv,
                            ty_ast: args[1].clone(),
                        },
                    );
                    None
                }
                "load_global" => {
                    // Load value from a global variable
                    let varname = match &args[0] {
                        Expr::Ident(n) => n.clone(),
                        _ => panic!("load_global(global variable)"),
                    };
                    let alloca = self
                        .global_variables
                        .get(&varname)
                        .expect(&format!("Undefined _global variable {}", &varname)); // safe because variable must exist
                    Some((
                        self.builder
                            .build_load(
                                self.llvm_type_from_expr(&alloca.ty_ast),
                                alloca.ptr.as_pointer_value(),
                                &varname,
                            )
                            .unwrap()
                            .into(),
                        alloca.ty_ast.clone(),
                        Some(VariablesPointerAndTypes {
                            ptr: alloca.ptr.as_pointer_value(),
                            typeexpr: alloca.ty_ast.clone(),
                        }),
                    ))
                }
                "=_global" => match &args[1] {
                    // Assignment to a global variable
                    // Array assign is special
                    Expr::ArrayLiteral(elems) => Some((
                        self.codegen_array_assign(&args[0], elems, variables)
                            .unwrap(), // safe unwrap: codegen_array_assign returns Some for valid array literals
                        Expr::Ident("void".to_string()),
                        None, // Array assignment does not return a value because the pointer already exists
                    )),
                    _ => {
                        let varname = match &args[0] {
                            Expr::Ident(n) => n.clone(),
                            _ => panic!("load_global(global variable)"),
                        };
                        let value = self.compile_expr(&args[1], variables).unwrap();
                        let alloca = self
                            .global_variables
                            .get(&varname)
                            .expect(&format!("Undefined _global variable {}", &varname))
                            .ptr;
                        self.builder
                            .build_store(alloca.as_pointer_value(), value.0)
                            .unwrap();
                        None
                    }
                },
                // declaring and initializing local variables
                "let" | "#=" => {
                    // args: [var_name, initial_value, type_name]
                    let var_name = match &args[0] {
                        Expr::Ident(s) => s,
                        _ => panic!("let: first arg must be variable name"),
                    };

                    let llvm_type: BasicTypeEnum = self.llvm_type_from_expr(&args[2]);

                    // If there is an initial value
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
                    // Check if an initial value is provided; "_" means no initial value
                    // If no initial value, zero-initialize (works for numeric types and structs)
                    let init_val = if init_val_exist {
                        self.compile_expr(&args[1], variables)
                    } else {
                        // For structs, initialize with zeroed
                        Some((
                            llvm_type.const_zero(),
                            Expr::Ident("void".to_string()),
                            None,
                        ))
                    };

                    // alloca
                    let alloca = self
                        .builder
                        .build_alloca(llvm_type, var_name)
                        .expect("fail alloca");
                    self.builder
                        .build_store(alloca, init_val.clone().unwrap().0) // safe unwrap: the existence of init_val is already checked
                        .unwrap();
                    variables.insert(
                        var_name.clone(),
                        VariablesPointerAndTypes {
                            ptr: alloca,
                            typeexpr: args[2].clone(),
                        },
                    );
                    // if its type is pointer with ownership, record ownership scope and the entire function ownership
                    match &args[2] {
                        Expr::TypeApply { base, args } if base == "*" => {
                            self.current_owners.insert(var_name.clone(), true);
                            self.scope_owners.set_true(var_name.clone());
                            if !init_val_exist {
                                println!(
                                    "{}: {var_name} is Owner (*:{:?}). it must have value: at function {}",
                                    "Error".red().bold(),
                                    args,
                                    self.current_fn
                                        .as_ref()
                                        .expect("return: out of function")
                                        .fn_name
                                        .clone(),
                                )
                            }
                        }
                        Expr::TypeApply { base: _, args: _ } => {}
                        _ => {
                            if init_val_exist && !type_match(&args[2], &init_val.clone().unwrap().1)
                            {
                                println!(
                                    "{}: {var_name} Type miss match : expected {:?} found {:?}: at function {}",
                                    "Error".red().bold(),
                                    &args[2],
                                    &init_val.clone().unwrap().1,
                                    self.current_fn
                                        .as_ref()
                                        .expect("return: out of function")
                                        .fn_name
                                        .clone(),
                                )
                            }
                        }
                    }

                    Some((init_val.unwrap().0, Expr::Ident("void".to_string()), None))
                }
                // Assignment
                "=" => match &args[1] {
                    // Array assign is special
                    Expr::ArrayLiteral(elems) => Some((
                        self.codegen_array_assign(&args[0], elems, variables)
                            .unwrap(), // safe unwrap: codegen_array_assign returns Some for valid array literals
                        Expr::Ident("void".to_string()),
                        None, // Array assignment does not return a value because the pointer already exists
                    )),
                    _ => {
                        let value = self.compile_expr(&args[1], variables).unwrap();
                        let alloca = self.get_pointer_expr(&args[0], variables);

                        // Check for immutable borrow: cannot reassign *_(immutable) or val(immutable)
                        if let Expr::Call { name: _name, args } = &args[0]
                            && let Some(Expr::Ident(s)) = args.get(0)
                            && let Some(val) = variables.get(s)
                            && let Expr::TypeApply { base, args: _args } = &val.typeexpr
                            && base == "&"
                        {
                            println!(
                                "{} :{} :{:?} is immutable borrow! if you want to reassign, use &mut:T : at function {}",
                                "Error".red().bold(),
                                s,
                                &val.typeexpr,
                                self.current_fn
                                    .as_ref()
                                    .expect("return: out of function")
                                    .fn_name
                                    .clone(),
                            );
                        }
                        self.builder.build_store(alloca, value.0).unwrap();
                        Some(value)
                    }
                },
                // binary operations: add, sub, mul, div, rem
                "+" | "-" | "*" | "/" | "%" => {
                    let lhs_val = self.compile_expr(&args[0], variables)?;
                    let rhs_val = self.compile_expr(&args[1], variables)?;

                    // ===== Type matching: Match the right side to the type of the left side =====
                    let rhs_casted = match (lhs_val.0, rhs_val.0) {
                        // ------- When both the left and right are integers -------
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

                        // ------- When both the left and right are floating point -------
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

                        // ------- int + float → float (left is float)-------
                        (BasicValueEnum::FloatValue(l), BasicValueEnum::IntValue(r)) => {
                            let r2 = self
                                .builder
                                .build_signed_int_to_float(r, l.get_type(), "i2f")
                                .unwrap();
                            BasicValueEnum::FloatValue(r2)
                        }

                        // ------- int + float → int (left is int)-------
                        (BasicValueEnum::IntValue(l), BasicValueEnum::FloatValue(r)) => {
                            let r2 = self
                                .builder
                                .build_float_to_signed_int(r, l.get_type(), "f2i")
                                .unwrap();
                            BasicValueEnum::IntValue(r2)
                        }

                        _ => panic!("Unsupported combination in binary operation"),
                    };

                    // ===== Calculation from here =====
                    // Perform the arithmetic operation: operands are now type-matched (int or float)
                    let result = match (lhs_val.0, rhs_casted) {
                        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                            let v = match name.as_str() {
                                "+" => self.builder.build_int_add(l, r, "add").unwrap(),
                                "-" => self.builder.build_int_sub(l, r, "sub").unwrap(),
                                "*" => self.builder.build_int_mul(l, r, "mul").unwrap(),
                                "/" => self.builder.build_int_signed_div(l, r, "div").unwrap(),
                                "%" => self.builder.build_int_signed_rem(l, r, "rem").unwrap(),
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
                                "%" => self.builder.build_float_rem(l, r, "frem").unwrap(),
                                _ => unreachable!(),
                            };
                            v.as_basic_value_enum()
                        }

                        _ => unreachable!(),
                    };

                    Some((result, lhs_val.1, None))
                }
                // float Special Functions (Intrinsics)
                "sqrt" | "cos" | "sin" | "pow" | "exp" | "log" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap().0;
                            val
                        })
                        .collect();
                    // check first argument
                    let val_l = match compiled_args[0] {
                        BasicValueEnum::FloatValue(v) => v,
                        // unwrap is safe here because only float arguments are allowed for these intrinsics
                        _ => panic!("{} requires float values", name.as_str()),
                    };
                    // If the function takes a second argument (e.g., pow), ensure it is a float
                    let val_r = if compiled_args.len() > 1 {
                        match compiled_args[1] {
                            BasicValueEnum::FloatValue(v) => Some(v),
                            // unwrap is safe here because only float arguments are allowed for these intrinsics
                            _ => panic!("at {} expected float", name.as_str()),
                        }
                    } else {
                        None
                    };
                    match name.as_str() {
                        "sqrt" => Some((
                            self.call_intrinsic("llvm.sqrt.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        "cos" => Some((
                            self.call_intrinsic("llvm.cos.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        "sin" => Some((
                            self.call_intrinsic("llvm.sin.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        "pow" => Some((
                            self.call_intrinsic("llvm.pow.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        "exp" => Some((
                            self.call_intrinsic("llvm.exp.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        "log" => Some((
                            self.call_intrinsic("llvm.log.f64", val_l, val_r),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        _ => None,
                    }
                }
                // Compile binary comparison operations. Returns boolean i1 value.
                "==" | "!=" | "<=" | ">=" | "<" | ">" => {
                    let compiled_args: Vec<BasicValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap().0;
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
                    Some((
                        v.expect("Unsupported comparison operator"),
                        Expr::Ident("bool".to_string()),
                        None,
                    ))
                }
                // Bitwise Operations
                //and,or,xor,left shift,right shift(sign extend is true),right shift(sign extend is false)
                "&" | "|" | "^" | "<<" | ">>" | "l>>" => {
                    let compiled_args: Vec<(
                        BasicValueEnum,
                        Expr,
                        Option<VariablesPointerAndTypes>,
                    )> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let val_l = match compiled_args[0].0 {
                        BasicValueEnum::IntValue(v) => v,
                        _ => panic!("{} requires integer values", name.as_str()),
                    };
                    let val_r = match compiled_args[1].0 {
                        BasicValueEnum::IntValue(v) => v,
                        _ => panic!("{} requires integer values", name.as_str()),
                    };
                    // compiled_args[0].clone().1 is type of first argument
                    // return type is type of  first argument
                    match name.as_str() {
                        "&" => Some((
                            self.builder
                                .build_and(val_l, val_r, "and_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        "|" => Some((
                            self.builder
                                .build_or(val_l, val_r, "or_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        "^" => Some((
                            self.builder
                                .build_xor(val_l, val_r, "xor_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        "<<" => Some((
                            self.builder
                                .build_left_shift(val_l, val_r, "<<_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        ">>" => Some((
                            self.builder
                                .build_right_shift(val_l, val_r, true, ">>_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        "l>>" => Some((
                            self.builder
                                .build_right_shift(val_l, val_r, false, "l>>_tmp")
                                .unwrap()
                                .as_basic_value_enum(),
                            compiled_args[0].clone().1,
                            None,
                        )),
                        _ => panic!("Unsupported bitwise operator: {}", name),
                    }
                }
                // Bool Operations
                "&&" | "and" => self.build_andand(
                    args.get(0).expect("&& need 2 args"),
                    args.get(1).expect("&& need 2 args"),
                    variables,
                ),
                // &&& = andand, || = or
                "&&&" | "||" | "andand" | "or" => {
                    let compiled_args: Vec<(
                        BasicValueEnum,
                        Expr,
                        Option<VariablesPointerAndTypes>,
                    )> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let lhs_i1 = match compiled_args[0].0 {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("{} requires boolean values", name.as_str()),
                    };
                    let rhs_i1 = match compiled_args[1].0 {
                        BasicValueEnum::IntValue(v) if v.get_type().get_bit_width() == 1 => v,
                        _ => panic!("{} requires boolean values", name.as_str()),
                    };
                    let v = match name.as_str() {
                        "&&&" | "andand" => Some(
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
                    Some((v.unwrap(), Expr::Ident("bool".to_string()), None))
                }
                // Bit Reverse
                "!" | "not" => {
                    let compiled_args: Vec<(BasicValueEnum, Expr, Option<_>)> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    let val_i1 = match compiled_args[0].0 {
                        BasicValueEnum::IntValue(v) => v,
                        _ => panic!("{} requires integer or boolean values", name.as_str()),
                    };
                    Some((
                        self.builder
                            .build_not(val_i1, "not_tmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        compiled_args[0].clone().1,
                        None,
                    ))
                }
                // Cast to numeric types
                // char, i32/i64, f32/f64 -> cast to i64 or f64
                // String -> parsed to i64 or f64
                "as_i64" | "as_f64" => {
                    let compiled_args: Vec<(BasicValueEnum, Expr, Option<_>)> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self
                                .compile_expr(arg, variables)
                                .expect("argument must have value");
                            val
                        })
                        .collect();

                    match name.as_str() {
                        "as_i64" => Some((
                            self.build_cast_to_i64(compiled_args[0].0)
                                .as_basic_value_enum(),
                            Expr::Ident("i64".to_string()),
                            None,
                        )),
                        "as_f64" => Some((
                            self.build_cast_to_f64(compiled_args[0].0)
                                .as_basic_value_enum(),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        _ => None,
                    }
                }
                "as" => {
                    let value = self
                        .compile_expr(&args[0], variables)
                        .expect("failed to compile expression for 'as'");
                    Some((
                        self.build_cast(value.0, value.2, &args[1], &value.1, false),
                        args[1].clone(),
                        None,
                    ))
                }
                "unsafe_as" => {
                    let value = self
                        .compile_expr(&args[0], variables)
                        .expect("failed to compile expression for 'unsafe_as'");
                    Some((
                        self.build_cast(value.0, value.2, &args[1], &value.1, true),
                        args[1].clone(),
                        None,
                    ))
                }
                // Deprecated names for backward compatibility
                // They behave the same as "as_i64" | "as_f64"
                // Kept to allow older code to continue working
                "parse_i64" | "parse_f64" => {
                    let compiled_args: Vec<(BasicValueEnum, Expr, Option<_>)> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    match name.as_str() {
                        "parse_i64" => Some((
                            self.build_cast_to_i64(compiled_args[0].0)
                                .as_basic_value_enum(),
                            Expr::Ident("i64".to_string()),
                            None,
                        )),
                        "parse_f64" => Some((
                            self.build_cast_to_f64(compiled_args[0].0)
                                .as_basic_value_enum(),
                            Expr::Ident("f64".to_string()),
                            None,
                        )),
                        _ => None,
                    }
                }
                // Pure choose expression: choose(cond, then_val, else_val)
                // Both 'then_val' and 'else_val' are always evaluated.
                // Avoid side effects in either branch.
                "choose" => {
                    let compiled_args: Vec<(BasicValueEnum, Expr, Option<_>)> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            val
                        })
                        .collect();
                    Some((
                        self.build_if_expr(
                            compiled_args[0].0,
                            compiled_args[1].0,
                            compiled_args[2].0,
                            "choose",
                        ),
                        compiled_args[1].clone().1,
                        None,
                    ))
                }
                // if expression: if(cond, then_val, else_val)
                "?" => self.build_if_no_ef_expr(
                    args.get(0).expect("?: failed get condition"),
                    args.get(1).expect("?: failed get then"),
                    args.get(2).expect("?: failed get else"),
                    variables,
                ),
                // Print with newline
                "println" => {
                    let s_val = self.compile_expr(&args[0], variables).unwrap();
                    let str_ptr = match s_val.0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("println expects string pointer"),
                    };
                    self.build_println_from_ptr(str_ptr);
                    None
                }
                // print without newline
                "print" => {
                    let s_val = self.compile_expr(&args[0], variables).unwrap();
                    let str_ptr = match s_val.0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("print expects string pointer"),
                    };
                    self.build_print_from_ptr(str_ptr);
                    None
                }
                // Wrapper of sprintf
                "format" => {
                    // string pointer
                    let fmt_val = self.compile_expr(&args[0], variables).unwrap();

                    let fmt_ptr = match fmt_val.0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("format expects string pointer"),
                    };
                    // remaining arguments: values to format
                    let arg_vals: Vec<_> = args[1..]
                        .iter()
                        .map(|a| self.compile_expr(a, variables).unwrap().0)
                        .collect();
                    // returns a string pointer containing the formatted string
                    let res_ptr = self.build_format_from_ptr(fmt_ptr, &arg_vals);
                    Some((
                        res_ptr.into(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![Expr::Ident("char".to_string())],
                        },
                        None,
                    ))
                }
                // Pointer addition in 1-byte units (like void* + n)
                "ptr_add_byte" => {
                    // args: [ptr, idx] or [idx, ptr]
                    let val_l = self.compile_expr(&args[0], variables).unwrap();
                    let val_r = self.compile_expr(&args[1], variables).unwrap();

                    let (ptr_val, idx_int) = match (val_l.0, val_r.0) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("ptr_add expects (ptr, int) or (int, ptr)"),
                    };
                    // GEP using i8 as element type => 1 byte unit
                    let gep = unsafe {
                        self.builder
                            .build_gep(
                                self.llvm_type_from_expr(&Expr::Ident("T".to_string())), // 1byte
                                ptr_val,
                                &[idx_int],
                                "ptr_add_byte",
                            )
                            .unwrap()
                    };
                    // return as ptr:T (like void*)
                    // borrow checker doesn't check "ptr" type pointer
                    Some((
                        gep.as_basic_value_enum(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![Expr::Ident("T".to_string())],
                        },
                        None,
                    ))
                }
                // Pointer addition in units of the pointed-to type
                // Returns the same pointer type as the left operand
                "ptr_add" => {
                    // args: [ptr, idx] or [idx, ptr]
                    let val_l = self.compile_expr(&args[0], variables).unwrap();
                    let val_r = self.compile_expr(&args[1], variables).unwrap();

                    let (ptr_val, idx_int) = match (val_l.0, val_r.0) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("ptr_add expects (ptr, int) or (int, ptr)"),
                    };
                    // GEP using the type pointed by left operand => adds in units of that type
                    let gep = unsafe {
                        self.builder
                            .build_gep(
                                self.llvm_type_from_expr(&expr_deref(&val_l.1)),
                                ptr_val,
                                &[idx_int],
                                "ptr_add",
                            )
                            .unwrap()
                    };

                    Some((gep.as_basic_value_enum(), val_l.1, None))
                }
                // index access (1-level): index(arr, idx) or index(idx, arr) -> load *(arr + idx)
                // for backward compatibility
                // non-multidimensional version of []
                "index" => {
                    // args: [ptr, idx] or [idx, ptr] -> load *(ptr + idx)
                    let a = self.compile_expr(&args[0], variables).unwrap();
                    let b = self.compile_expr(&args[1], variables).unwrap();
                    let (ptr_val, idx_int) = match (a.0, b.0) {
                        (BasicValueEnum::PointerValue(p), BasicValueEnum::IntValue(i)) => {
                            (p, self.int_to_i64(i))
                        }
                        (BasicValueEnum::IntValue(i), BasicValueEnum::PointerValue(p)) => {
                            (p, self.int_to_i64(i))
                        }
                        _ => panic!("index expects (ptr, int) or (int, ptr)"),
                    };

                    let gep = unsafe {
                        self.builder
                            .build_gep(
                                self.llvm_type_from_expr(&expr_deref(&a.1)),
                                ptr_val,
                                &[idx_int],
                                "idx_ptr",
                            )
                            .unwrap()
                    };
                    let loaded = self
                        .builder
                        .build_load(self.llvm_type_from_expr(&expr_deref(&a.1)), gep, "idx_load")
                        .unwrap();
                    Some((
                        loaded.as_basic_value_enum(),
                        expr_deref(&a.1),
                        Some(VariablesPointerAndTypes {
                            ptr: gep,
                            typeexpr: expr_deref(&a.1),
                        }),
                    ))
                }
                // multi-dimensional index access: [](arr, e1, e2, ...) = arr[e1][e2]...
                "[]" => {
                    let depth = args.len(); // number of indices + 1 (for array pointer)
                    let arr_pointer = self.compile_expr(&args[0], variables).unwrap();
                    let ptr_val = match arr_pointer.0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("[] expect ptr,i64,i64,... found {:?}", arr_pointer.1),
                    };

                    let mut last_ptr = ptr_val;
                    let mut typeexp = arr_pointer.1;
                    for i in 1..depth {
                        let (val, ty, _p) = self.compile_expr(&args[i], variables).unwrap();
                        let idx_int = match val {
                            BasicValueEnum::IntValue(i_val) => self.int_to_i64(i_val),
                            _ => panic!("[] expect ptr,i64,i64,... found {:?}", ty),
                        };
                        let gep = unsafe {
                            self.builder
                                .build_gep(
                                    self.llvm_type_from_expr(&expr_deref(&typeexp)),
                                    last_ptr,
                                    &[idx_int],
                                    "idx_ptr",
                                )
                                .unwrap()
                        };
                        if i == depth - 1 {
                            last_ptr = gep;
                            break;
                        }
                        last_ptr = self
                            .builder
                            .build_load(
                                self.llvm_type_from_expr(&expr_deref(&typeexp)),
                                gep,
                                "idx_load",
                            )
                            .unwrap()
                            .into_pointer_value();
                        typeexp = expr_deref(&typeexp);
                    }
                    typeexp = expr_deref(&typeexp);
                    let loaded = self
                        .builder
                        .build_load(self.llvm_type_from_expr(&typeexp), last_ptr, "idx[]_load")
                        .unwrap();
                    Some((
                        loaded.as_basic_value_enum(),
                        typeexp.clone(),
                        Some(VariablesPointerAndTypes {
                            ptr: last_ptr,
                            typeexpr: typeexp.clone(),
                        }),
                    ))
                }
                "[array]" => {
                    let depth = args.len();

                    // arr represents a value of type [T;N]
                    let (arr_val, arr_ty, arr_ptr_opt) =
                        self.compile_expr(&args[0], variables).unwrap();

                    let arr_ptr = match &arr_ty {
                        Expr::TypeApply { base, args: _ } if base.starts_with("array_") => {
                            arr_ptr_opt.expect("[array] requires array lvalue").ptr
                        }
                        Expr::TypeApply { base, args: _ }
                            if base == "ptr" || base == "&" || base == "&mut" || base == "*" =>
                        {
                            arr_val.into_pointer_value()
                        }
                        _ => panic!("[array] requires pointer or array_N:T"),
                    };

                    let mut last_ptr = arr_ptr;
                    let mut typeexp = arr_ty.clone();

                    for i in 1..depth {
                        let (idx_val, _idx_ty, _) = self.compile_expr(&args[i], variables).unwrap();

                        let idx_int = match idx_val {
                            BasicValueEnum::IntValue(i) => self.int_to_i64(i),
                            _ => panic!("[array] index must be int"),
                        };

                        match &typeexp {
                            // [T;N]
                            Expr::TypeApply { base, args: _ } if base.starts_with("array_") => {
                                let gep = unsafe {
                                    self.builder.build_gep(
                                        self.llvm_type_from_expr(&typeexp),
                                        last_ptr,
                                        &[self.context.i32_type().const_zero(), idx_int],
                                        "array_idx",
                                    )
                                }
                                .unwrap();

                                last_ptr = match expr_deref(&typeexp) {
                                    Expr::TypeApply { base, args: _ }
                                        if base.starts_with("array_") =>
                                    {
                                        gep
                                    }
                                    Expr::TypeApply { base, args: _ }
                                        if base == "ptr"
                                            || base == "&"
                                            || base == "&mut"
                                            || base == "*" =>
                                    {
                                        if i == depth - 1 {
                                            last_ptr = gep;
                                            typeexp = expr_deref(&typeexp);
                                            break;
                                        }
                                        self.builder
                                            .build_load(
                                                self.llvm_type_from_expr(&expr_deref(&typeexp)),
                                                gep,
                                                "idx_load",
                                            )
                                            .unwrap()
                                            .into_pointer_value()
                                    }
                                    _ => gep,
                                };

                                typeexp = expr_deref(&typeexp);
                            }

                            // ptr<T>
                            Expr::TypeApply { base, args: _ }
                                if base == "ptr"
                                    || base == "&"
                                    || base == "&mut"
                                    || base == "*" =>
                            {
                                let gep = unsafe {
                                    self.builder.build_gep(
                                        self.llvm_type_from_expr(&expr_deref(&typeexp)),
                                        last_ptr,
                                        &[idx_int],
                                        "ptr_idx",
                                    )
                                }
                                .unwrap();
                                last_ptr = match expr_deref(&typeexp) {
                                    Expr::TypeApply { base, args: _ }
                                        if base.starts_with("array_") =>
                                    {
                                        gep
                                    }
                                    Expr::TypeApply { base, args: _ }
                                        if base == "ptr"
                                            || base == "&"
                                            || base == "&mut"
                                            || base == "*" =>
                                    {
                                        if i == depth - 1 {
                                            last_ptr = gep;
                                            typeexp = expr_deref(&typeexp);
                                            break;
                                        }
                                        self.builder
                                            .build_load(
                                                self.llvm_type_from_expr(&expr_deref(&typeexp)),
                                                gep,
                                                "idx_load",
                                            )
                                            .unwrap()
                                            .into_pointer_value()
                                    }
                                    _ => gep,
                                };
                                typeexp = expr_deref(&typeexp);
                            }

                            _ => panic!("[array] indexing non-array type {:?}", typeexp),
                        }
                    }

                    let loaded = self
                        .builder
                        .build_load(self.llvm_type_from_expr(&typeexp), last_ptr, "array_load")
                        .unwrap();

                    Some((
                        loaded.into(),
                        typeexp.clone(),
                        Some(VariablesPointerAndTypes {
                            ptr: last_ptr,
                            typeexpr: typeexp,
                        }),
                    ))
                }
                "array" => self.build_array_value(variables, args),
                // Allocate memory on the stack
                "alloc_array" | "alloc" | "salloc" => {
                    // args: [type_name, length_expr]

                    // type of elements
                    let elem_ty = self.llvm_type_from_expr(&args[0]);

                    // capacity = IntValue
                    let len_val = self.compile_expr(&args[1], variables).unwrap();
                    let len_val = match len_val.0 {
                        BasicValueEnum::IntValue(i) => i,
                        _ => panic!("alloc_array: length must be integer"),
                    };

                    let array_ptr = self
                        .builder
                        .build_array_alloca(elem_ty, len_val, "array")
                        .unwrap();

                    // Return as ptr:elem_ty
                    Some((
                        array_ptr.as_basic_value_enum(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![args[0].clone()],
                        },
                        None,
                    ))
                }
                "free" => {
                    self.compile_free(&args[0], variables);
                    None
                }
                // transfer ownership of a *:T variable
                "pmove" => {
                    let (val, ty, ptr) = self.compile_expr(&args[0], variables).unwrap();
                    match &ty {
                        // Expect variable identifier
                        Expr::TypeApply { base, args: _ } if base == "*" => {
                            if let Expr::Ident(var_name) = &args[0] {
                                // Check if variable has already been moved
                                if let Some(owned) = self.current_owners.get_mut(var_name) {
                                    if !*owned {
                                        println!(
                                            "{} : at pmove \"{}\" already moved : at function {}",
                                            "Error".red().bold(),
                                            var_name,
                                            self.current_fn
                                                .as_ref()
                                                .expect("return: out of function")
                                                .fn_name
                                                .clone(),
                                        );
                                    }
                                    *owned = false;
                                }
                                if let Some(owned) = self.scope_owners.owners[self.scope_owners.pos]
                                    .get_mut(var_name)
                                {
                                    *owned = false;
                                }
                            }
                        }
                        Expr::TypeApply { base, args: _args } if base == "&" || base == "&mut" => {
                            println!(
                                "{} pmove expect *:T variables found {:?} : at function {}",
                                "Error".red().bold(),
                                &ty,
                                self.current_fn
                                    .as_ref()
                                    .expect("return: out of function")
                                    .fn_name
                                    .clone(),
                            );
                        }
                        _ => {
                            println!(
                                "{} pmove expect *:T variables found {:?} : at function {}",
                                "Error".red().bold(),
                                &args[0],
                                self.current_fn
                                    .as_ref()
                                    .expect("return: out of function")
                                    .fn_name
                                    .clone(),
                            );
                        }
                    }
                    Some((val, ty, ptr))
                }
                // immutable borrow
                "p&" => {
                    let (val, ty, ptr) = self.compile_expr(&args[0], variables).unwrap();
                    Some((
                        val,
                        Expr::TypeApply {
                            base: "&".to_string(),
                            args: vec![expr_deref(&ty)],
                        },
                        ptr,
                    ))
                }
                // mutable borrow
                "p&mut" => {
                    let (val, ty, ptr) = self.compile_expr(&args[0], variables).unwrap();
                    match &ty {
                        Expr::TypeApply { base, args: _args } if base == "&" => {
                            println!(
                                "{}: p&mut expect &mut:T or *:T found {:?} {:?} : at function {}",
                                "Error".red().bold(),
                                ty,
                                &args[0],
                                self.current_fn
                                    .as_ref()
                                    .expect("return: out of function")
                                    .fn_name
                                    .clone(),
                            )
                        }
                        _ => {}
                    }
                    Some((
                        val,
                        Expr::TypeApply {
                            base: "&mut".to_string(),
                            args: vec![expr_deref(&ty)],
                        },
                        ptr,
                    ))
                }
                // malloc(size, elements_type): safety wrapper for C's malloc
                // requires the pointed-to type for type safety
                "malloc" => {
                    let compiled_size = self.compile_expr(&args[0], variables).unwrap();
                    let size = match compiled_size.0 {
                        BasicValueEnum::IntValue(i) => Some(i),
                        _ => panic!("malloc expects integer size"),
                    };
                    let ele_type = self.llvm_type_from_expr(&args[1]);
                    // return ptr:elements_type
                    Some((
                        self.compile_malloc(size.unwrap(), ele_type)
                            .as_basic_value_enum(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![args[1].clone()],
                        },
                        None,
                    ))
                }
                // realloc(ptr, size, elements_type): safety wrapper for C's realloc
                // requires the pointed-to type for type safety
                "realloc" => {
                    let ptr = match self.compile_expr(&args[0], variables).unwrap().0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("realloc expects (ptr,size,type)"),
                    };
                    let compiled_size = self.compile_expr(&args[1], variables).unwrap();
                    let size = match compiled_size.0 {
                        BasicValueEnum::IntValue(i) => Some(i),
                        _ => panic!("realloc expects (ptr,size,type)"),
                    };
                    let ele_type = self.llvm_type_from_expr(&args[2]);
                    // return ptr:elements_type
                    Some((
                        self.compile_realloc(ptr, size.unwrap(), ele_type)
                            .as_basic_value_enum(),
                        Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![args[2].clone()],
                        },
                        None,
                    ))
                }
                // memcpy(dest,src,size)
                "memcpy" => {
                    let dest_ptr = self.compile_expr(&args[0], variables).unwrap();
                    let src_ptr = self.compile_expr(&args[1], variables).unwrap();
                    let size = self.compile_expr(&args[2], variables).unwrap();
                    let (dest_i8, src_i8, size_int) = match (dest_ptr.0, src_ptr.0, size.0) {
                        (
                            BasicValueEnum::PointerValue(d),
                            BasicValueEnum::PointerValue(s),
                            BasicValueEnum::IntValue(i),
                        ) => (
                            self.builder
                                .build_pointer_cast(
                                    d,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "dest8",
                                )
                                .unwrap(),
                            self.builder
                                .build_pointer_cast(
                                    s,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "dest8",
                                )
                                .unwrap(),
                            i,
                        ),
                        _ => panic!(
                            "memcpy expects (ptr,ptr,int) found {:?},{:?},{:?}",
                            dest_ptr.1, src_ptr.1, size.1
                        ),
                    };
                    self.builder
                        .build_memcpy(dest_i8, 1, src_i8, 1, size_int)
                        .unwrap();

                    None
                }
                // memmove(dest,src,size)
                "memmove" => {
                    let dest_ptr = self.compile_expr(&args[0], variables).unwrap();
                    let src_ptr = self.compile_expr(&args[1], variables).unwrap();
                    let size = self.compile_expr(&args[2], variables).unwrap();
                    let (dest_i8, src_i8, size_int) = match (dest_ptr.0, src_ptr.0, size.0) {
                        (
                            BasicValueEnum::PointerValue(d),
                            BasicValueEnum::PointerValue(s),
                            BasicValueEnum::IntValue(i),
                        ) => (
                            self.builder
                                .build_pointer_cast(
                                    d,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "dest8",
                                )
                                .unwrap(),
                            self.builder
                                .build_pointer_cast(
                                    s,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "dest8",
                                )
                                .unwrap(),
                            i,
                        ),
                        _ => panic!(
                            "memmove expects (ptr,ptr,int)found {:?},{:?},{:?}",
                            dest_ptr.1, src_ptr.1, size.1
                        ),
                    };
                    self.builder
                        .build_memmove(dest_i8, 1, src_i8, 1, size_int)
                        .unwrap();

                    None
                }
                // memmove(dest,value,size)
                "memset" => {
                    let dest_ptr = self.compile_expr(&args[0], variables).unwrap();
                    let src_val = self.compile_expr(&args[1], variables).unwrap();
                    let size = self.compile_expr(&args[2], variables).unwrap();
                    let (dest_i8, value, size_int) = match (dest_ptr.0, src_val.0, size.0) {
                        (
                            BasicValueEnum::PointerValue(d),
                            BasicValueEnum::IntValue(v),
                            BasicValueEnum::IntValue(s),
                        ) => (
                            self.builder
                                .build_pointer_cast(
                                    d,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "dest8",
                                )
                                .unwrap(),
                            v,
                            s,
                        ),
                        _ => panic!("memset expects (ptr,val,int)"),
                    };
                    let value_i8 = self
                        .builder
                        .build_int_truncate(value, self.context.i8_type(), "val8")
                        .unwrap();
                    self.builder
                        .build_memset(dest_i8, 1, value_i8, size_int)
                        .unwrap();

                    None
                }
                // return the size of a type in bytes as an isize
                "sizeof" => Some((
                    self.compile_sizeof(&args[0]),
                    Expr::Ident("isize".to_string()),
                    None,
                )),
                // === struct member access ===
                // "_>": struct pointer -> member value (dereference pointer, then load member)
                // ".": struct value -> member value (use alloca of struct value, then load member)
                // "->": struct pointer -> member pointer (get pointer to member, do not load)
                // struct member access
                // struct pointer -> member value
                "_>" => {
                    let value_struct = self.compile_expr(&args[0], variables).unwrap();
                    let (memberptr, membertype) =
                        self.compile_member_access(value_struct.0, &args[1], &value_struct.1);
                    let member_value = self
                        .builder
                        .build_load(
                            self.llvm_type_from_expr(&expr_deref(&membertype)),
                            memberptr,
                            "getmembervalue",
                        )
                        .unwrap();
                    Some((
                        member_value.into(),
                        expr_deref(&membertype),
                        Some(VariablesPointerAndTypes {
                            ptr: memberptr,
                            typeexpr: expr_deref(&membertype),
                        }),
                    ))
                }
                // struct member access
                // struct value -> member value
                "." => {
                    let value_struct = self.compile_expr(&args[0], variables).unwrap();
                    let alloca =
                        self.ensure_lvalue(value_struct.0, &value_struct.1, value_struct.2); //(value_struct.2.unwrap().ptr) or (temp_value alloc)
                    let (memberptr, membertype) = self.compile_member_access(
                        alloca.as_basic_value_enum(),
                        &args[1],
                        &Expr::TypeApply {
                            base: "ptr".to_string(),
                            args: vec![value_struct.1],
                        },
                    );
                    let member_value = self
                        .builder
                        .build_load(
                            self.llvm_type_from_expr(&expr_deref(&membertype)),
                            memberptr,
                            "getmembervalue",
                        )
                        .unwrap();
                    Some((
                        member_value.into(),
                        expr_deref(&membertype),
                        Some(VariablesPointerAndTypes {
                            ptr: memberptr,
                            typeexpr: expr_deref(&membertype),
                        }),
                    ))
                }
                // struct member access
                // struct pointer -> member pointer
                "->" => {
                    let value_struct = self.compile_expr(&args[0], variables).unwrap();
                    let (memberptr, membertype) =
                        self.compile_member_access(value_struct.0, &args[1], &value_struct.1);
                    Some((memberptr.as_basic_value_enum().into(), membertype, None))
                }
                // wrapper of C scanf
                "scanf" => {
                    let fmt_val = self.compile_expr(&args[0], variables).unwrap();
                    // Remaining arguments: values to format
                    let fmt_ptr = match fmt_val.0 {
                        BasicValueEnum::PointerValue(p) => p,
                        _ => panic!("scanf expects string pointer"),
                    };
                    let arg_vals: Vec<_> = args[1..]
                        .iter()
                        .map(|a| self.compile_expr(a, variables).unwrap().0)
                        .collect();
                    self.build_scan_from_ptr(fmt_ptr, &arg_vals);
                    None
                }
                // cast to generic pointer
                "to_anyptr" => {
                    // to_anyptr(ptr)
                    let ptr = self.compile_expr(&args[0], variables);
                    match ptr.unwrap().0 {
                        BasicValueEnum::PointerValue(p) => Some((
                            self.compile_to_anyptr(p, variables).as_basic_value_enum(),
                            Expr::TypeApply {
                                base: "ptr".to_string(),
                                args: vec![Expr::Ident("T".to_string())],
                            },
                            None,
                        )),
                        _ => None,
                    }
                }
                // cast from generic pointer
                "from_anyptr" => {
                    // from_anyptr(ptr,pointed type)
                    let ptr = self.compile_expr(&args[0], variables);
                    let target_type = self.llvm_type_from_expr(&args[1]);
                    match (ptr.unwrap().0, target_type) {
                        (BasicValueEnum::PointerValue(p), BasicTypeEnum::PointerType(target)) => {
                            Some((
                                self.compile_from_anyptr(p, target).as_basic_value_enum(),
                                Expr::TypeApply {
                                    base: "ptr".to_string(),
                                    args: vec![args[1].clone()],
                                },
                                None,
                            ))
                        }
                        _ => None,
                    }
                }
                "unsafe_asm" => Some((
                    self.compile_unsafe_asm(args, variables),
                    args[0].clone(),
                    None,
                )),
                "trap" => {
                    // declare or retrieve the llvm.trap intrinsic
                    let trap_fn = match self.module.get_function("llvm.trap") {
                        Some(f) => f,
                        None => self.module.add_function(
                            "llvm.trap",
                            self.context.void_type().fn_type(&[], false),
                            None,
                        ),
                    };
                    self.builder.build_call(trap_fn, &[], "").unwrap();
                    self.builder.build_unreachable().unwrap();
                    None
                }
                // Call other functions
                name => {
                    // Lookup LLVM function by name.
                    let func = self
                        .module
                        .get_function(&name)
                        .expect(&format!("Function {} not found", name));
                    // Compile each argument and convert to BasicMetadataValueEnum,
                    // which is required by build_call.
                    let compiled_args: Vec<BasicMetadataValueEnum> = args
                        .into_iter()
                        .map(|arg| {
                            let val = self.compile_expr(arg, variables).unwrap();
                            BasicMetadataValueEnum::from(val.0)
                        })
                        .collect();
                    // Emit LLVM call instruction
                    let call_site = self
                        .builder
                        .build_call(func, &compiled_args, "calltmp")
                        .unwrap();
                    // If the function has a return value, extract it.
                    if func.get_type().get_return_type().is_some() {
                        Some((
                            call_site.try_as_basic_value().basic().unwrap(),
                            // return type is stored in function_types map (AST-level type)
                            self.function_types
                                .get(name)
                                .expect(&format!("not defined function {}", name))
                                .0
                                .clone(),
                            None,
                        ))
                    } else {
                        None // return is void
                    }
                }
            },

            _ => None,
        }
    }
    /// Builds an equality comparison.
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
                let intptr_ty = self.context.i64_type(); // Assume 64bit
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::EQ, a_i, b_i, "eq")
                    .unwrap()
            }
            _ => panic!("Unsupported types for =="),
        }
    }
    /// Builds a not-equal comparison.
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
                let intptr_ty = self.context.i64_type(); // Assume 64bit
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::NE, a_i, b_i, "neq")
                    .unwrap()
            }
            _ => panic!("Unsupported types for !="),
        }
    }
    /// Builds a less-than comparison.
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
                let intptr_ty = self.context.i64_type(); // Assume 64bit
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::SLT, a_i, b_i, "slt")
                    .unwrap()
            }
            _ => panic!("Unsupported types for <"),
        }
    }
    /// Builds a greater-than comparison.
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
                let intptr_ty = self.context.i64_type(); // Assume 64bit
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::SGT, a_i, b_i, "sgt")
                    .unwrap()
            }
            _ => panic!("Unsupported types for >"),
        }
    }
    /// Builds a less-than-or-equal comparison.
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
                let intptr_ty = self.context.i64_type(); // Assume 64bit
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::SLE, a_i, b_i, "sle")
                    .unwrap()
            }
            _ => panic!("Unsupported types for <="),
        }
    }
    /// Builds a greater-than-or-equal comparison.
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
                let intptr_ty = self.context.i64_type(); // 64bit 前提
                let a_i = self
                    .builder
                    .build_ptr_to_int(a, intptr_ty, "ptr_a_i")
                    .unwrap();
                let b_i = self
                    .builder
                    .build_ptr_to_int(b, intptr_ty, "ptr_b_i")
                    .unwrap();

                self.builder
                    .build_int_compare(IntPredicate::SGE, a_i, b_i, "sge")
                    .unwrap()
            }
            _ => panic!("Unsupported types for >="),
        }
    }
    /// Extends a smaller integer to i64.
    fn int_to_i64(&self, v: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64t = self.context.i64_type();
        if v.get_type() == i64t {
            v
        } else {
            // sign extension (if from i32 etc.)
            self.builder.build_int_s_extend(v, i64t, "idx_i64").unwrap()
        }
    }
    /// Resolves an LLVM type from a WapL type expression.
    fn llvm_type_from_expr(&self, expr: &Expr) -> BasicTypeEnum<'ctx> {
        match expr {
            Expr::Ident(name) => {
                if let Some(st) = self.struct_types.get(name) {
                    st.as_basic_type_enum()
                } else {
                    match name.as_str() {
                        "isize" => {
                            if self.bitsize.as_str() == "32" {
                                self.context.i32_type().into()
                            } else {
                                self.context.i64_type().into()
                            }
                        }
                        "i32" => self.context.i32_type().into(),
                        "i64" => self.context.i64_type().into(),
                        "f32" => self.context.f32_type().into(),
                        "f64" => self.context.f64_type().into(),
                        "bool" => self.context.bool_type().into(),
                        "char" => self.context.i8_type().into(),
                        "T" => self.context.i8_type().into(),
                        "ptr" => self.context.ptr_type(Default::default()).into(),
                        "*" => self.context.ptr_type(Default::default()).into(),
                        "&" => self.context.ptr_type(Default::default()).into(),
                        "&mut" => self.context.ptr_type(Default::default()).into(),
                        "&n" => self.context.ptr_type(Default::default()).into(),
                        _ => panic!("Unknown type: {}", name),
                    }
                }
            }
            Expr::TypeApply { base, args } => {
                if let Some(len_str) = base.strip_prefix("array_") {
                    if args.len() != 1 {
                        panic!("array_N:T requires exactly one type argument");
                    }

                    let len: u32 = len_str
                        .parse()
                        .unwrap_or_else(|_| panic!("Invalid array size in type: {}", base));

                    let elem_ty = self.llvm_type_from_expr(&args[0]);
                    return elem_ty.array_type(len).into();
                }
                match base.as_str() {
                    "ptr" | "*" | "&" | "&mut" | "&n" => {
                        if args.len() != 1 {
                            panic!("ptr<T> requires exactly one type argument");
                        }
                        let _elem_ty = self.llvm_type_from_expr(&args[0]);
                        self.context.ptr_type(Default::default()).into()
                    }
                    _ => panic!("Unknown type constructor: {}", base),
                }
            }
            _ => panic!(
                "Expected identifier type {:?} : at function {}",
                expr,
                self.current_fn
                    .as_ref()
                    .expect("return: out of function")
                    .fn_name
                    .clone(),
            ),
        }
    }
    /// Builds an array initializer or value.
    fn build_array_value(
        &mut self,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
        args: &[Expr],
    ) -> Option<(
        BasicValueEnum<'ctx>,
        Expr,
        Option<VariablesPointerAndTypes<'ctx>>,
    )> {
        let mut values = Vec::new();
        let mut elem_ty: Option<Expr> = None;
        for arg in args {
            let (val, ty, _ptr) = self.compile_expr(arg, variables).unwrap();

            if let Some(ref expected) = elem_ty {
                if !type_match(expected, &ty) {
                    self.println_error_message(&format!(
                        "array() type mismatch. expected {:?} found {:?}",
                        expected, &ty
                    ));
                }
            } else {
                elem_ty = Some(ty.clone());
            }

            values.push(val);
        }
        let n = values.len() as u32;
        let llvm_elem_ty = self.llvm_type_from_expr(elem_ty.as_ref().unwrap());
        let llvm_array_ty = llvm_elem_ty.array_type(n);
        let is_const = values.iter().all(|v| v.is_const());
        if is_const {
            let const_array = match llvm_elem_ty {
                BasicTypeEnum::IntType(t) => t
                    .const_array(
                        &values
                            .iter()
                            .map(|v| v.into_int_value())
                            .collect::<Vec<_>>(),
                    )
                    .as_basic_value_enum(),
                BasicTypeEnum::FloatType(t) => t
                    .const_array(
                        &values
                            .iter()
                            .map(|v| v.into_float_value())
                            .collect::<Vec<_>>(),
                    )
                    .as_basic_value_enum(),
                BasicTypeEnum::ArrayType(t) => t
                    .const_array(
                        &values
                            .iter()
                            .map(|v| v.into_array_value())
                            .collect::<Vec<_>>(),
                    )
                    .as_basic_value_enum(),
                BasicTypeEnum::StructType(t) => t
                    .const_array(
                        &values
                            .iter()
                            .map(|v| v.into_struct_value())
                            .collect::<Vec<_>>(),
                    )
                    .as_basic_value_enum(),
                _ => panic!("unsupported const array element type"),
            };
            return Some((
                const_array.into(),
                Expr::TypeApply {
                    base: format!("array_{}", n),
                    args: vec![elem_ty.unwrap()],
                },
                None,
            ));
        }
        let tmp = self
            .builder
            .build_alloca(llvm_array_ty, "array_tmp")
            .unwrap();

        for (i, v) in values.iter().enumerate() {
            let idx = self.context.i32_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder.build_gep(
                    llvm_array_ty,
                    tmp,
                    &[self.context.i32_type().const_zero(), idx],
                    "array_elem",
                )
            }
            .unwrap();

            self.builder.build_store(elem_ptr, *v).unwrap();
        }

        let array_val = self
            .builder
            .build_load(llvm_array_ty, tmp, "array_val")
            .unwrap();
        Some((
            array_val.into(),
            Expr::TypeApply {
                base: format!("array_{}", n),
                args: vec![elem_ty.unwrap()],
            },
            None,
        ))
    }
    /// Builds a formatted string using sprintf.
    pub fn build_format_from_ptr(
        &mut self,
        fmt_ptr: PointerValue<'ctx>,
        arg_vals: &[BasicValueEnum<'ctx>],
    ) -> PointerValue<'ctx> {
        // Create or retrieve the sprintf function
        let sprintf_fn = match self.module.get_function("sprintf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.ptr_type(Default::default());
                let fn_type = self
                    .context
                    .i32_type()
                    .fn_type(&[i8ptr_type.into(), i8ptr_type.into()], true); // variadic
                self.module.add_function("sprintf", fn_type, None)
            }
        };

        // Convert arguments to BasicMetadataValueEnum for variadic call
        let meta_args: Vec<BasicMetadataValueEnum> = arg_vals
            .iter()
            .map(|v| BasicMetadataValueEnum::from(*v))
            .collect();

        // Create a buffer to store the result of sprintf
        let buf = self
            .builder
            .build_alloca(self.context.i8_type().array_type(128), "fmt_buf")
            .unwrap();

        // Cast buffer to i8* for sprintf
        let buf_ptr = self
            .builder
            .build_bit_cast(
                buf,
                self.context.ptr_type(AddressSpace::from(0)),
                "fmt_buf_ptr",
            )
            .unwrap()
            .into_pointer_value();

        // Prepare arguments: buffer pointer + format string + variadic arguments
        let mut call_args = vec![buf_ptr.into(), fmt_ptr.into()];
        call_args.extend(meta_args.iter().cloned());

        // call sprintf
        self.builder
            .build_call(sprintf_fn, &call_args, "sprintf")
            .unwrap();

        // return buffer pointer as i8*
        buf_ptr
    }
    /// Builds a call to printf with a newline.
    pub fn build_println_from_ptr(&mut self, str_ptr: PointerValue<'ctx>) {
        // Create or retrieve the printf function
        let printf_fn = match self.module.get_function("printf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.ptr_type(Default::default());
                let fn_type = self.context.i32_type().fn_type(&[i8ptr_type.into()], true);
                self.module.add_function("printf", fn_type, None)
            }
        };

        // Create a new string literal for formatting with a newline
        let global_str_with_newline = self
            .builder
            .build_global_string_ptr(
                "%s\n", // display string and newline
                &format!("println_fmt_{}", self.str_counter),
            )
            .unwrap();
        self.str_counter += 1;

        // Prepare printf arguments
        let args: &[BasicMetadataValueEnum] = &[
            global_str_with_newline.as_pointer_value().into(),
            str_ptr.into(),
        ];

        // call printf
        self.builder.build_call(printf_fn, args, "printf").unwrap();
    }
    /// Builds a call to printf without a newline.
    pub fn build_print_from_ptr(&mut self, str_ptr: PointerValue<'ctx>) {
        // Create or retrieve the printf function
        let printf_fn = match self.module.get_function("printf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.ptr_type(Default::default());
                let fn_type = self.context.i32_type().fn_type(&[i8ptr_type.into()], true);
                self.module.add_function("printf", fn_type, None)
            }
        };

        // Create a new string literal for printing
        let global_str = self
            .builder
            .build_global_string_ptr(
                "%s", // display string
                &format!("println_fmt_{}", self.str_counter),
            )
            .unwrap();
        self.str_counter += 1;

        // Prepare printf arguments
        let args: &[BasicMetadataValueEnum] =
            &[global_str.as_pointer_value().into(), str_ptr.into()];

        // call printf
        self.builder.build_call(printf_fn, args, "printf").unwrap();
    }
    /// Builds a call to scanf.
    pub fn build_scan_from_ptr(
        &mut self,
        fmt_ptr: PointerValue<'ctx>,
        arg_vals: &[BasicValueEnum<'ctx>],
    ) -> IntValue<'ctx> {
        // Create or retrieve the scanf function
        let scanf_fn = match self.module.get_function("scanf") {
            Some(f) => f,
            None => {
                let i8ptr_type = self.context.ptr_type(Default::default());
                let fn_type = self.context.i32_type().fn_type(&[i8ptr_type.into()], true); // variadic
                self.module.add_function("scanf", fn_type, None)
            }
        };

        // Prepare arguments: format string + pointers to variables
        let mut call_args: Vec<BasicMetadataValueEnum> = vec![fmt_ptr.into()];

        call_args.extend(arg_vals.iter().map(|v| BasicMetadataValueEnum::from(*v)));

        // call scanf
        let ret = self
            .builder
            .build_call(scanf_fn, &call_args, "scanf")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap();

        ret.into_int_value()
    }
    /// Builds a ternary-style if-select expression.
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

        // Type consistency check and casting if necessary
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

    /// Builds an if-else control flow expression using basic blocks and PHI nodes.
    fn build_if_no_ef_expr(
        &mut self,
        cond_expr: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> Option<(
        BasicValueEnum<'ctx>,
        Expr,
        Option<VariablesPointerAndTypes<'ctx>>,
    )> {
        let (cond_val, cond_ty, _) = self.compile_expr(cond_expr, variables)?;
        match cond_ty {
            Expr::Ident(s) if s != "bool" => {
                println!(
                    "{:?} need to be bool because it is condition of if",
                    cond_expr
                )
            }
            _ => {}
        }
        let cond_i1 = cond_val.into_int_value();

        let current_bb = self.builder.get_insert_block().unwrap();
        let function = current_bb.get_parent().unwrap();

        let then_bb = self.context.append_basic_block(function, "if.then");
        let else_bb = self.context.append_basic_block(function, "if.else");
        let merge_bb = self.context.append_basic_block(function, "if.merge");
        // Branching
        self.builder
            .build_conditional_branch(cond_i1, then_bb, else_bb)
            .unwrap();
        // then
        self.builder.position_at_end(then_bb);
        let (then_val, then_ty, _) = self.compile_expr(then_expr, variables)?;
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let then_end = self.builder.get_insert_block().unwrap();
        // else
        self.builder.position_at_end(else_bb);
        let (else_val, else_ty, _) = self.compile_expr(else_expr, variables)?;
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let else_end = self.builder.get_insert_block().unwrap();

        let result_ty = match (then_ty.clone(), else_ty.clone()) {
            (Expr::Ident(thenty), Expr::Ident(elsety)) => {
                if thenty != elsety {
                    println!("if type miss match then:{},else:{}", thenty, elsety);
                    then_ty.clone()
                } else {
                    then_ty.clone()
                }
            }
            _ => then_ty.clone(),
        };
        // merge
        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(self.llvm_type_from_expr(&result_ty), "if.result")
            .expect("if phi failed");

        phi.add_incoming(&[(&then_val, then_end), (&else_val, else_end)]);

        Some((phi.as_basic_value(), result_ty, None))
    }

    /// Calls an LLVM intrinsic function.
    fn call_intrinsic(
        &mut self,
        intrinsic_name: &str,
        arg: FloatValue<'ctx>,
        arg2_some: Option<FloatValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match arg2_some {
            None => {
                // Expecting f64
                let f64_type = self.module.get_context().f64_type();

                // LLVM intrinsic function signature
                let fn_type = f64_type.fn_type(&[f64_type.into()], false);

                // declare or retrieve the intrinsic
                let func = match self.module.get_function(intrinsic_name) {
                    Some(f) => f,
                    None => self.module.add_function(intrinsic_name, fn_type, None),
                };

                // invocation
                let call = self
                    .builder
                    .build_call(func, &[arg.into()], "callintrinsic")
                    .unwrap();

                call.try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_float_value()
                    .as_basic_value_enum()
            }
            Some(arg2) => {
                // Expecting f64
                let f64_type = self.module.get_context().f64_type();

                // LLVM intrinsic function signature
                let fn_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);

                // declare or retrieve the intrinsic
                let func = match self.module.get_function(intrinsic_name) {
                    Some(f) => f,
                    None => self.module.add_function(intrinsic_name, fn_type, None),
                };

                // invocation
                let call = self
                    .builder
                    .build_call(func, &[arg.into(), arg2.into()], "callintrinsic")
                    .unwrap();

                call.try_as_basic_value()
                    .basic()
                    .unwrap()
                    .into_float_value()
                    .as_basic_value_enum()
            }
        }
    }

    /// Builds a type cast between different LLVM types.
    fn build_cast(
        &self,
        value: BasicValueEnum<'ctx>,
        value_ptr: Option<VariablesPointerAndTypes<'ctx>>,
        to_ty: &Expr,
        from_ty: &Expr,
        is_unsafe: bool,
    ) -> BasicValueEnum<'ctx> {
        let to_ty_llvm = self.llvm_type_from_expr(to_ty);
        let from_ty_llvm = self.llvm_type_from_expr(from_ty);
        match (to_ty_llvm, from_ty_llvm) {
            // int to int
            (BasicTypeEnum::IntType(to), BasicTypeEnum::IntType(from)) => {
                let v = value.into_int_value();
                if from.get_bit_width() < to.get_bit_width() {
                    self.builder
                        .build_int_s_extend(v, to, "sext")
                        .unwrap()
                        .into()
                } else if from.get_bit_width() > to.get_bit_width() {
                    self.builder
                        .build_int_truncate(v, to, "trunc")
                        .unwrap()
                        .into()
                } else {
                    v.into()
                }
            }
            // float to float
            (BasicTypeEnum::FloatType(to), BasicTypeEnum::FloatType(from)) => {
                let to_bw = float_bit_width(to);
                let from_bw = float_bit_width(from);
                let v = value.into_float_value();
                if from_bw < to_bw {
                    self.builder.build_float_ext(v, to, "fext").unwrap().into()
                } else if from_bw > to_bw {
                    self.builder
                        .build_float_trunc(v, to, "ftrunc")
                        .unwrap()
                        .into()
                } else {
                    v.into()
                }
            }
            // int to float
            (BasicTypeEnum::FloatType(to), BasicTypeEnum::IntType(_)) => self
                .builder
                .build_signed_int_to_float(value.into_int_value(), to, "sitofp")
                .unwrap()
                .into(),
            // float to int
            (BasicTypeEnum::IntType(to), BasicTypeEnum::FloatType(_)) => self
                .builder
                .build_float_to_signed_int(value.into_float_value(), to, "sitofp")
                .unwrap()
                .into(),
            (BasicTypeEnum::PointerType(to), BasicTypeEnum::IntType(_)) => {
                if !is_unsafe {
                    self.println_error_message(" \"as\" can not cast ptr type. use \"unsafe_as\"");
                }
                self.builder
                    .build_int_to_ptr(value.into_int_value(), to, "int_to_ptr")
                    .unwrap()
                    .into()
            }
            (BasicTypeEnum::IntType(to), BasicTypeEnum::PointerType(_)) => {
                if !is_unsafe {
                    self.println_error_message(" \"as\" can not cast ptr type. use \"unsafe_as\"");
                }
                self.builder
                    .build_ptr_to_int(value.into_pointer_value(), to, "ptr_to_int")
                    .unwrap()
                    .into()
            }
            (BasicTypeEnum::PointerType(to), BasicTypeEnum::PointerType(_)) => {
                if !is_unsafe {
                    self.println_error_message(" \"as\" can not cast ptr type. use \"unsafe_as\"");
                }
                self.builder
                    .build_bit_cast(value.into_pointer_value(), to, "ptr_cast")
                    .unwrap()
                    .into()
            }
            (BasicTypeEnum::PointerType(to), BasicTypeEnum::ArrayType(arr_ty)) => {
                let array_ptr = if let Some(ptr) = value_ptr {
                    ptr.ptr
                } else {
                    // rvalue -> create temporary space and copy
                    let tmp = self.builder.build_alloca(arr_ty, "array_tmp").unwrap();
                    self.builder.build_store(tmp, value).unwrap();
                    tmp
                };
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        arr_ty,
                        array_ptr,
                        &[
                            self.context.i32_type().const_zero(),
                            self.context.i32_type().const_zero(),
                        ],
                        "array_to_ptr",
                    )
                }
                .unwrap();
                self.builder
                    .build_bit_cast(elem_ptr, to, "array_to_ptr_cast")
                    .unwrap()
                    .into()
            }
            _ => panic!("unsupported cast type"),
        }
    }

    fn build_cast_to_i64(&self, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        let _f64_type = self.context.f64_type();

        match value {
            BasicValueEnum::IntValue(v) => {
                // possible i32 to i64 widening
                if v.get_type().get_bit_width() < 64 {
                    self.builder
                        .build_int_s_extend(v, i64_type, "i32_to_i64")
                        .unwrap()
                } else {
                    v
                }
            }

            BasicValueEnum::FloatValue(v) => {
                // f64 -> i64 (truncation)
                self.builder
                    .build_float_to_signed_int(v, i64_type, "f64_to_i64")
                    .unwrap()
            }

            BasicValueEnum::PointerValue(ptr) => {
                // Treat as string (i8*) and convert using strtol
                // strtol(i8*, i8**, i32)
                let strtol = self
                    .module
                    .get_function("strtol")
                    .expect("strtol not defined");

                let null_endptr = self.context.ptr_type(Default::default()).const_zero();
                let _null_endptr_ptr = self.context.ptr_type(Default::default()).const_zero();

                let endptr = self
                    .builder
                    .build_alloca(self.context.ptr_type(Default::default()), "endptr")
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
                    .basic()
                    .unwrap();

                result.into_int_value()
            }

            _ => panic!("Unsupported type for as_i64"),
        }
    }
    fn build_cast_to_f64(&self, value: BasicValueEnum<'ctx>) -> FloatValue<'ctx> {
        let f64_type = self.context.f64_type();
        let _i64_type = self.context.i64_type();

        match value {
            BasicValueEnum::IntValue(v) => {
                // int to float
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
                    .basic()
                    .unwrap();

                result.into_float_value()
            }

            _ => panic!("Unsupported type for as_f64"),
        }
    }

    pub fn gen_point(&mut self, name: &str) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");

        // Create a basic block for the label
        let block = self.context.append_basic_block(current_fn.function, name);

        // Register the label
        current_fn.labels.insert(name.to_string(), block);

        // Process unresolved jumps
        if let Some(jumps) = current_fn.unresolved.remove(name) {
            for pending in jumps {
                self.builder.position_at_end(pending.from);
                self.builder.build_unconditional_branch(block).unwrap();
            }
        }

        // Move the builder to this label
        self.builder.position_at_end(block);
    }

    pub fn gen_warpto(&mut self, name: &str) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");
        let current_block = self.builder.get_insert_block().unwrap();

        // If the label exists, jump immediately
        if let Some(&target) = current_fn.labels.get(name) {
            self.builder.build_unconditional_branch(target).unwrap();
            return;
        }

        // Label not defined yet -> register in unresolved
        current_fn
            .unresolved
            .entry(name.to_string())
            .or_default()
            .push(PendingJump {
                from: current_block,
            });

        // Don't create a dummy block or move the builder
        // The next point will set the builder's position
    }

    pub fn gen_warptoif(
        &mut self,
        cond: IntValue<'ctx>,
        label_true: &str,
        label_false: Option<&str>,
    ) {
        let current_fn = self.current_fn.as_mut().expect("Not in a function");
        let func = current_fn.function;
        let _from_block = self.builder.get_insert_block().unwrap();

        // ---- TRUE branch block ----
        let true_block = if let Some(&target) = current_fn.labels.get(label_true) {
            target
        } else {
            // label undefined -> create a pending block
            let pending = self.context.append_basic_block(func, "pending_true");
            current_fn
                .unresolved
                .entry(label_true.to_string())
                .or_default()
                .push(PendingJump { from: pending });

            pending
        };

        // ---- FALSE branch block ----
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
                // no false label: else part is a fall-through block
                self.context.append_basic_block(func, "else_block")
            }
        };

        // ---- Generate conditional branch ----
        self.builder
            .build_conditional_branch(cond, true_block, false_block)
            .unwrap();

        // warptoif only closes the block
        // builder does not move anywhere
        // -> the builder will be set at the next point
    }

    fn get_pointer_expr(
        &mut self,
        expr: &Expr,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> PointerValue<'ctx> {
        match expr {
            Expr::Ident(name) => {
                // Variable a -> raw pointer to that variable (alloca)
                variables
                    .get(name)
                    .expect(&format!("Undefined variable{}", name))
                    .ptr
            }

            Expr::Call { name, args } if name == "val" || name == "*_" => {
                // val(ptr) -> value of ptr (= pointer value)
                let p_val = self.compile_expr(&args[0], variables).unwrap().0;
                p_val.into_pointer_value()
            }

            Expr::Call { name, args } if name == "index" => {
                // allow index(ptr, idx) or index(idx, ptr) (n[p])
                let first = self.compile_expr(&args[0], variables).unwrap();
                let second = self.compile_expr(&args[1], variables).unwrap();

                // decide which is pointer
                if let BasicValueEnum::PointerValue(ptrv) = first.0 {
                    // first is pointer, second must be integer
                    let idx_int = match second.0 {
                        BasicValueEnum::IntValue(iv) => self.int_to_i64(iv),
                        _ => panic!("index: second arg must be integer"),
                    };
                    // GEP
                    let gep = unsafe {
                        self.builder
                            .build_gep(
                                self.llvm_type_from_expr(&first.1),
                                ptrv,
                                &[idx_int],
                                "idx_ptr",
                            )
                            .unwrap()
                    };
                    gep
                } else if let BasicValueEnum::PointerValue(ptrv) = second.0 {
                    // second is pointer, first is integer (n[p] style)
                    let idx_int = match first.0 {
                        BasicValueEnum::IntValue(iv) => self.int_to_i64(iv),
                        _ => panic!("index: first arg must be integer"),
                    };
                    let gep = unsafe {
                        self.builder
                            .build_gep(
                                self.llvm_type_from_expr(&second.1),
                                ptrv,
                                &[idx_int],
                                "idx_ptr",
                            )
                            .unwrap()
                    };
                    gep
                } else {
                    panic!("index: one of args must be pointer");
                }
            }
            Expr::Call { name: _, args: _ } => {
                let (p_val, _ty, ispointer) = self.compile_expr(expr, variables).unwrap();
                match ispointer {
                    Some(p) => p.ptr,
                    None => p_val.into_pointer_value(),
                }
            }
            _ => panic!("Left-hand side must be a pointer or val(ptr)"),
        }
    }

    fn codegen_array_assign(
        &mut self,
        lhs: &Expr,
        elems: &Vec<Expr>,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        // lhs is a ptr type
        let array_ptr = self.get_pointer_expr_only_val(lhs, variables);

        for (i, elem) in elems.iter().enumerate() {
            let index_val = self.context.i64_type().const_int(i as u64, false);

            // Generate value
            let val = self.compile_expr(elem, variables).unwrap();

            let gep = unsafe {
                self.builder
                    .build_gep(
                        self.llvm_type_from_expr(&val.1),
                        array_ptr,    // ptr
                        &[index_val], // index
                        "array_idx",
                    )
                    .unwrap()
            };

            // store
            self.builder.build_store(gep, val.0).unwrap();
        }

        Some(array_ptr.as_basic_value_enum())
    }

    fn compile_malloc(
        &self,
        size: IntValue<'ctx>,
        _element_type: BasicTypeEnum<'ctx>, // i64_type, i8_type, f64_type etc.
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
            .basic()
            .unwrap()
            .into_pointer_value();

        // convert malloc return value (i8*) to required type T*
        let typed_ptr = self
            .builder
            .build_bit_cast(
                i8ptr,
                self.context.ptr_type(AddressSpace::default()),
                "cast_ptr",
            )
            .unwrap()
            .into_pointer_value();

        typed_ptr
    }
    fn compile_realloc(
        &self,
        old_ptr: PointerValue<'ctx>,
        size: IntValue<'ctx>,
        _element_type: BasicTypeEnum<'ctx>, // i64_type, i8_type, f64_type など
    ) -> PointerValue<'ctx> {
        let realloc_fn = self
            .module
            .get_function("realloc")
            .expect("realloc not defined");
        let i8ptr = self
            .builder
            .build_call(realloc_fn, &[old_ptr.into(), size.into()], "realloc_call")
            .unwrap()
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();

        // convert realloc return value (i8*) to required type T*
        let typed_ptr = self
            .builder
            .build_bit_cast(
                i8ptr,
                self.context.ptr_type(AddressSpace::default()),
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
        struct_type: &Expr,
    ) -> (PointerValue<'ctx>, Expr) {
        let field_name = match field_name_expr {
            Expr::Ident(s) => s.clone(),
            _ => "".to_string(),
        };
        let struct_value = match struct_value_enum {
            BasicValueEnum::PointerValue(ptr_val) => ptr_val,
            _ => {
                // Error handling: not a pointer value
                panic!("Member access must be performed on a pointer value.");
            }
        };
        let struct_type: StructType<'ctx> = self
            .llvm_type_from_expr(&expr_deref(&struct_type))
            .into_struct_type();

        let key = self
            .struct_types
            .iter()
            .find(|(_, v)| **v == struct_type)
            .map(|(k, _)| k.clone());
        let struct_type_name = key.unwrap();
        let (indx, typeexpr) = self.struct_fields[&struct_type_name]
            .iter()
            .find(|v| v.0 == field_name.as_str())
            .map(|v| (v.2, v.3.clone()))
            .unwrap();
        let return_ptr = self
            .builder
            .build_struct_gep(struct_type, struct_value, indx, "access")
            .unwrap();
        (
            return_ptr,
            Expr::TypeApply {
                base: "ptr".to_string(),
                args: vec![typeexpr],
            },
        )
    }

    fn compile_sizeof(&self, ty_expr: &Expr) -> BasicValueEnum<'ctx> {
        let llvm_ty = self.llvm_type_from_expr(ty_expr);

        let size_i64 = llvm_ty.size_of().expect("LLVM failed to compute size_of");

        let isize_ty = self.llvm_type_from_expr(&Expr::Ident("isize".to_string()));

        let size_isize = self
            .builder
            .build_int_cast(size_i64, isize_ty.into_int_type(), "sizeof_isize")
            .unwrap();

        size_isize.as_basic_value_enum()
    }
    fn compile_to_anyptr(
        &mut self,
        ptr: PointerValue<'ctx>,
        _variables: &HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> PointerValue<'ctx> {
        let i8_ptr = self.context.ptr_type(AddressSpace::from(0));
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
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) {
        // Get pointer value
        let free_ptr: PointerValue<'_> = self
            .compile_expr(ptr_expr, variables)
            .unwrap()
            .0
            .into_pointer_value();
        // bitcast to i8*
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
        let casted = self
            .builder
            .build_bit_cast(free_ptr, i8_ptr_type, "free_cast")
            .unwrap()
            .into_pointer_value();
        let free_fn = self.module.get_function("free").expect("free not defined");
        // call free
        self.builder
            .build_call(free_fn, &[casted.into()], "free_call")
            .unwrap();
    }
    fn get_pointer_expr_only_val(
        &mut self,
        ptr_expr: &Expr,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> PointerValue<'ctx> {
        let ptr = self.get_pointer_expr(ptr_expr, variables);

        let ptr_val = match ptr_expr {
            Expr::Ident(_) => self
                .builder
                .build_load(self.context.ptr_type(Default::default()), ptr, "get_ptr")
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
            self.llvm_type_from_expr(&Expr::Ident("isize".to_string()))
                .fn_type(
                    &[
                        context.ptr_type(Default::default()).into(),
                        context.ptr_type(Default::default()).into(),
                        context.i32_type().into(),
                    ],
                    false,
                ),
            None,
        );

        // atof(i8*)
        self.module.add_function(
            "atof",
            context
                .f64_type()
                .fn_type(&[context.ptr_type(Default::default()).into()], false),
            None,
        );

        // printf(i8*, ...)
        self.module.add_function(
            "printf",
            context
                .i32_type()
                .fn_type(&[context.ptr_type(Default::default()).into()], true),
            None,
        );

        // sprintf(i8*, i8*, ...)
        self.module.add_function(
            "sprintf",
            context.i32_type().fn_type(
                &[
                    context.ptr_type(Default::default()).into(),
                    context.ptr_type(Default::default()).into(),
                ],
                true,
            ),
            None,
        );

        //realloc(size,type)
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = self.llvm_type_from_expr(&Expr::Ident("isize".to_string()));
        let realloc_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i64_type.into()], false);
        let _realloc_fn = self.module.add_function("realloc", realloc_type, None);

        //malloc(size,type)
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = self.llvm_type_from_expr(&Expr::Ident("isize".to_string()));
        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
        let _malloc_fn = self.module.add_function("malloc", malloc_type, None);

        //free(ptr)
        let void_type = self.context.void_type();
        let i8_ptr = self.context.ptr_type(AddressSpace::default());
        let fn_type = void_type.fn_type(&[i8_ptr.into()], false);
        self.module.add_function("free", fn_type, None);

        //scanf
        let i8ptr_type = context.ptr_type(AddressSpace::default());
        let scanf_type = context.i32_type().fn_type(&[i8ptr_type.into()], true);
        self.module.add_function("scanf", scanf_type, None);
    }
    fn compile_declare(&mut self, func: Declare) {
        if self.function_types.contains_key(&func.name) && self.function_types[&func.name].1 {
            panic!("function '{}' already defined", func.name);
        }
        // --- type of return value ---
        let return_type_is_void = matches!(func.return_type, Expr::Ident(ref s) if s == "void");

        let return_type_enum = if return_type_is_void {
            None
        } else {
            Some(self.llvm_type_from_expr(&func.return_type))
        };

        // --- type of arguments ---
        let arg_types: Vec<BasicTypeEnum> = func
            .args
            .iter()
            .map(|ty| self.llvm_type_from_expr(ty))
            .collect();

        // ---convert to Metadata type ---
        let arg_types_meta: Vec<BasicMetadataTypeEnum> =
            arg_types.iter().map(|t| (*t).into()).collect();

        // --- LLVM gen function ---
        let fn_type = if return_type_is_void {
            self.context
                .void_type()
                .fn_type(&arg_types_meta, func.is_vararg)
        } else {
            return_type_enum
                .unwrap()
                .fn_type(&arg_types_meta, func.is_vararg)
        };

        // --- add function ---
        let _llvm_func = self.module.add_function(&func.name, fn_type, None);
        self.function_types
            .insert(func.name.clone(), (func.return_type, false));
    }

    fn compile_loopif(
        &mut self,
        name: &str,
        cond: &Expr,
        body: &Vec<Stmt>,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) {
        let mut inloop_variables = variables.clone();
        self.scope_owners.next();
        let current_fn = self.current_fn.as_mut().expect("Not in a function");
        let loop_start = self
            .context
            .append_basic_block(current_fn.function, &format!("loop_start-{}", name)); // Avoid access by warpto in code
        self.builder.build_unconditional_branch(loop_start).unwrap(); // loop-start for returning from unresolved jump resolution
        let cond_block = self
            .context
            .append_basic_block(current_fn.function, &format!("continue-{}", name));
        let body_block = self
            .context
            .append_basic_block(current_fn.function, &format!("no_judge_continue-{}", name));
        let end_block = self
            .context
            .append_basic_block(current_fn.function, &format!("break-{}", name));
        // Register labels
        current_fn
            .labels
            .insert(format!("continue-{}", name), cond_block);
        current_fn
            .labels
            .insert(format!("no_judge_continue-{}", name), body_block);
        current_fn
            .labels
            .insert(format!("break-{}", name), end_block);
        // Process unresolved jumps
        if let Some(jumps) = current_fn.unresolved.remove(&format!("continue-{}", name)) {
            for pending in jumps {
                self.builder.position_at_end(pending.from);
                self.builder.build_unconditional_branch(cond_block).unwrap();
            }
        }
        if let Some(jumps) = current_fn
            .unresolved
            .remove(&format!("no_judge_continue-{}", name))
        {
            for pending in jumps {
                self.builder.position_at_end(pending.from);
                self.builder.build_unconditional_branch(body_block).unwrap();
            }
        }
        if let Some(jumps) = current_fn.unresolved.remove(&format!("break-{}", name)) {
            for pending in jumps {
                self.builder.position_at_end(pending.from);
                self.builder.build_unconditional_branch(end_block).unwrap();
            }
        }
        self.builder.position_at_end(loop_start); // Always go to the loop start
        // Jump to condition block first
        self.builder.build_unconditional_branch(cond_block).unwrap();
        // Condition block
        self.builder.position_at_end(cond_block);
        let cond_int_value = match self.compile_expr(&cond, &mut inloop_variables).unwrap().0 {
            BasicValueEnum::IntValue(i) => i,
            _ => panic!("Loopif conditions must have exactly i1 value"),
        };
        // conditional branch
        self.builder
            .build_conditional_branch(cond_int_value, body_block, end_block)
            .unwrap();
        // Body block
        self.builder.position_at_end(body_block);
        // body processing
        for stmt in body {
            let _value = self.compile_stmt(&stmt, &mut inloop_variables);
        }
        self.builder.build_unconditional_branch(cond_block).unwrap(); // Jump to condition again

        // end block
        self.builder.position_at_end(end_block);
        for i in self.scope_owners.show_current() {
            if let Some(b) = self.current_owners.get(&i.0)
                && *b
            {
                println!(
                    "{}:you need to free or drop pointer {}!: at function {}",
                    "Error".red().bold(),
                    i.0,
                    self.current_fn
                        .as_ref()
                        .expect("return: out of function")
                        .fn_name
                        .clone(),
                );
            }
        }
        self.scope_owners.reset_current();
        self.scope_owners.back();
    }
    fn build_andand(
        &mut self,
        lhs_expr: &Expr,
        rhs_expr: &Expr,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> Option<(
        BasicValueEnum<'ctx>,
        Expr,
        Option<VariablesPointerAndTypes<'ctx>>,
    )> {
        // evaluate lhs
        let lhs = self
            .compile_expr(lhs_expr, variables)
            .unwrap()
            .0
            .into_int_value();

        let current_bb = self.builder.get_insert_block().unwrap();
        let function = current_bb.get_parent().unwrap();

        let rhs_bb = self.context.append_basic_block(function, "and.rhs");
        let false_bb = self.context.append_basic_block(function, "and.false");
        let merge_bb = self.context.append_basic_block(function, "and.merge");

        // branch based on lhs
        self.builder
            .build_conditional_branch(lhs, rhs_bb, false_bb)
            .unwrap();

        // evaluate rhs
        self.builder.position_at_end(rhs_bb);
        let rhs = self
            .compile_expr(rhs_expr, variables)
            .unwrap()
            .0
            .into_int_value();
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let rhs_end = self.builder.get_insert_block().unwrap();

        // false side
        self.builder.position_at_end(false_bb);
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let false_end = self.builder.get_insert_block().unwrap();

        // merge
        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "and.result")
            .expect("\"&&\" failed phi");
        phi.add_incoming(&[
            (&rhs, rhs_end),
            (&self.context.bool_type().const_int(0, false), false_end),
        ]);
        Some((phi.as_basic_value(), Expr::Ident("bool".to_string()), None))
    }
    fn compile_if(
        &mut self,
        branches: Vec<IfBranch>,
        else_block: Option<Vec<Stmt>>,
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> bool {
        let current_fn = self
            .current_fn
            .as_mut()
            .expect("Not in a function")
            .function;
        let mut end_bb: Option<BasicBlock<'_>> = None;
        let else_exist = match else_block {
            Some(_) => true,
            None => false,
        };
        let mut end_bb_exist = false;
        let mut cond_bbs = Vec::new();
        let mut then_bbs = Vec::new();
        for i in 0..branches.len() {
            cond_bbs.push(
                self.context
                    .append_basic_block(current_fn, &format!("if.cond.{i}")),
            );
            then_bbs.push(
                self.context
                    .append_basic_block(current_fn, &format!("if.then.{i}")),
            );
        }
        let else_bb = if else_block.is_some() {
            Some(self.context.append_basic_block(current_fn, "if.else"))
        } else {
            None
        };
        let mut all_unreachable = true;
        self.builder
            .build_unconditional_branch(cond_bbs[0])
            .unwrap(); // jump to if condition
        for (i, branch) in branches.iter().enumerate() {
            if branch.cond.len() != 1 {
                println!("if or elif: conditions must have exactly one value");
            }
            let mut inif_variables = variables.clone(); // new scope
            self.scope_owners.next(); // push ownership map stack
            self.builder.position_at_end(cond_bbs[i]); // go to condition expression
            let cond = match self
                .compile_expr(&branches[i].cond[0], &mut inif_variables)
                .unwrap()
                .0
            {
                BasicValueEnum::IntValue(i) => i,
                _ => panic!("if conditions must have exactly i1 value"),
            };

            let false_target = if i + 1 < branches.len() {
                cond_bbs[i + 1]
            } else {
                if !end_bb_exist && !else_exist {
                    end_bb = Some(self.context.append_basic_block(current_fn, "if.end"));
                    end_bb_exist = true;
                }
                if else_exist {
                    else_bb.unwrap()
                } else {
                    end_bb.unwrap()
                }
                //else_bb.unwrap_or(end_bb.unwrap())
            };

            self.builder
                .build_conditional_branch(cond, then_bbs[i], false_target)
                .unwrap();
            self.builder.position_at_end(then_bbs[i]);
            let mut reachable = true;
            for stmt in &branch.body {
                let reached = self.compile_stmt(stmt, &mut inif_variables);
                if !reached {
                    reachable = false;
                }
            }
            if reachable {
                if !end_bb_exist {
                    end_bb = Some(self.context.append_basic_block(current_fn, "if.end"));
                    end_bb_exist = true;
                }
                self.builder
                    .build_unconditional_branch(end_bb.unwrap())
                    .unwrap();
                all_unreachable = false;
            }
            for i in self.scope_owners.show_current() {
                if let Some(b) = self.current_owners.get(&i.0)
                    && *b
                {
                    println!(
                        "{}:you need to free or drop pointer {}! : at function {}",
                        "Error".red().bold(),
                        i.0,
                        self.current_fn
                            .as_ref()
                            .expect("return: out of function")
                            .fn_name
                            .clone(),
                    );
                }
            }
            self.scope_owners.reset_current();
            self.scope_owners.back(); // pop stack
        }
        let mut else_reachable = true;
        if let Some(else_bb) = else_bb {
            let mut inif_variables = variables.clone();
            self.scope_owners.next();
            self.builder.position_at_end(else_bb);

            for stmt in &else_block.unwrap() {
                let reached = self.compile_stmt(stmt, &mut inif_variables);
                if !reached {
                    else_reachable = false;
                }
            }
            if else_reachable {
                if !end_bb_exist {
                    end_bb = Some(self.context.append_basic_block(current_fn, "if.end"));
                    end_bb_exist = true;
                }
                self.builder
                    .build_unconditional_branch(end_bb.unwrap())
                    .unwrap();
            }
            for i in self.scope_owners.show_current() {
                if let Some(b) = self.current_owners.get(&i.0)
                    && *b
                {
                    println!(
                        "{}:you need to free or drop pointer {}! : at function {}",
                        "Error".red().bold(),
                        i.0,
                        self.current_fn
                            .as_ref()
                            .expect("return: out of function")
                            .fn_name
                            .clone(),
                    );
                }
            }
            self.scope_owners.reset_current();
            self.scope_owners.back(); // pop stack
        }
        if else_reachable || !all_unreachable {
            self.builder.position_at_end(end_bb.unwrap());
        }
        end_bb_exist
    }
    fn eval_const_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::String(s) => s.clone(),
            _ => panic!("unsafe_asm requires string literal"),
        }
    }
    fn println_error_message(&self, msg: &str) {
        println!(
            "{}: {} : at function {}",
            "Error".red().bold(),
            msg,
            self.current_fn
                .as_ref()
                .expect("return: out of function")
                .fn_name
                .clone(),
        );
    }
    fn compile_unsafe_asm(
        &mut self,
        args: &[Expr],
        variables: &mut HashMap<String, VariablesPointerAndTypes<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        const IDX_RET: usize = 0;
        const IDX_ASM: usize = 1;
        const IDX_CONSTRAINT: usize = 2;
        const IDX_SIDEEFFECT: usize = 3;
        const IDX_DIALECT: usize = 4;
        const IDX_INPUTS: usize = 5;
        assert!(args.len() >= 5, "unsafe_asm needs >= 5 arguments");

        // ==============================
        // asm template / constraint
        // ==============================
        let asm_template = self.eval_const_string(&args[IDX_ASM]);
        let constraint = self.eval_const_string(&args[IDX_CONSTRAINT]);
        let sideef = match &args[IDX_SIDEEFFECT] {
            Expr::Bool(b) => *b,
            _ => panic!("unsafe_asm side effect requires bool literal"),
        };

        // ==============================
        // Input arguments
        // ==============================
        let mut input_values: Vec<BasicMetadataValueEnum<'_>> = Vec::new();
        let mut input_types: Vec<BasicMetadataTypeEnum> = Vec::new();

        for expr in &args[IDX_INPUTS..] {
            let val = self.compile_expr(expr, variables).unwrap().0;
            input_types.push(val.get_type().into());
            input_values.push(val.into());
        }

        // ==============================
        // Return value type
        // ==============================
        // unsafe_asm is determined by the "expected type"
        let return_type_is_void = matches!(args[IDX_RET], Expr::Ident(ref s) if s == "void");

        let return_type_enum = if return_type_is_void {
            None
        } else {
            Some(self.llvm_type_from_expr(&args[IDX_RET]))
        };
        let fn_type = if return_type_is_void {
            self.context.void_type().fn_type(&input_types, false)
        } else {
            return_type_enum.unwrap().fn_type(&input_types, false)
        };
        let dialect = match &args[IDX_DIALECT] {
            Expr::String(s) => match s.as_str() {
                "intel" => Some(InlineAsmDialect::Intel),
                "att" => Some(InlineAsmDialect::ATT),
                _ => None,
            },
            _ => panic!("asm dialect must be string literal"),
        };

        // ==============================
        // Create InlineAsm
        // ==============================
        let inline_asm = self.context.create_inline_asm(
            fn_type,
            asm_template,
            constraint,
            sideef, // has_side_effects
            false,  // is_align_stack
            dialect,
            false, // can_throw
        );

        // ==============================
        // call
        // ==============================
        let call_site = self
            .builder
            .build_indirect_call(fn_type, inline_asm, &input_values, "asmcall")
            .unwrap();

        // ==============================
        // return value
        // ==============================
        match call_site.try_as_basic_value().basic() {
            Some(v) => v,
            None => {
                // void asm
                self.context.i64_type().const_zero().into()
            }
        }
    }
    fn ensure_lvalue(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: &Expr,
        opt: Option<VariablesPointerAndTypes<'ctx>>,
    ) -> PointerValue<'ctx> {
        if let Some(v) = opt {
            v.ptr
        } else {
            let temp = self
                .builder
                .build_alloca(self.llvm_type_from_expr(ty), "temp")
                .expect("fail to alloc temp_value");
            self.builder
                .build_store(temp, value)
                .expect("fail to store temp_value");
            temp
        }
    }
}
fn type_match(type1: &Expr, type2: &Expr) -> bool {
    match (type1, type2) {
        (Expr::Ident(ty1), Expr::Ident(ty2)) => {
            ty1.as_str() == ty2.as_str() || ty1.as_str() == "T" || ty2.as_str() == "T"
        }
        (
            Expr::TypeApply {
                base: base1,
                args: arg1,
            },
            Expr::TypeApply {
                base: base2,
                args: arg2,
            },
        ) => {
            (base1.as_str() == base2.as_str() || base1.as_str() == "ptr" || base2.as_str() == "ptr")
                && type_match(
                    arg1.get(0)
                        .expect(&format!("{}:{:?} has no args", "Error".red(), type1)),
                    arg2.get(0)
                        .expect(&format!("{}:{:?} has no args", "Error".red(), type2)),
                )
        }
        _ => false,
    }
}
fn float_bit_width(ft: FloatType) -> u32 {
    let printed = ft.print_to_string(); // hold ownership
    let name = printed.to_str().unwrap(); // OK to borrow
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
fn combine_toplevel<'ctx>(module: &Module<'ctx>, builder: &Builder<'ctx>, has_main: bool) {
    // 1. Collect <toplevel_child> functions in the module
    let mut toplevel_funcs: Vec<FunctionValue> = vec![];

    for f in module.get_functions() {
        let name = f.get_name().to_str().unwrap();
        if name.starts_with("toplevel_child") {
            toplevel_funcs.push(f);
        }
    }

    // 2. Create main function type: i32 main(i32, i8**)
    let i32_type = module.get_context().i32_type();
    let _i8_ptr_type = module.get_context().ptr_type(AddressSpace::from(0));
    let mut main_fn_type = i32_type.fn_type(
        &[
            i32_type.into(),
            module.get_context().ptr_type(AddressSpace::from(0)).into(),
        ],
        false,
    );
    if has_main {
        main_fn_type = i32_type.fn_type(&[], false);
    }
    // 3. add main function
    let main_fn = if !has_main {
        module.add_function("main", main_fn_type, None)
    } else {
        module.get_function("_TOPLEVEL_").unwrap()
    };
    if !has_main {
        let argc = main_fn.get_nth_param(0).unwrap();
        argc.set_name("argc");
        let argv = main_fn.get_nth_param(1).unwrap();
        argv.set_name("argv");
    }

    // 4. create entry block
    let entry_bb = module.get_context().append_basic_block(main_fn, "entry");
    builder.position_at_end(entry_bb);

    // 5. call toplevel_child in order
    for f in toplevel_funcs.iter() {
        builder
            .build_call(*f, &[], &format!("call_{}", f.get_name().to_str().unwrap()))
            .unwrap();
    }

    // 6. return i32 0
    builder
        .build_return(Some(&i32_type.const_int(0, false)))
        .unwrap();
}

fn get_var_alloca<'ctx>(
    variables: &HashMap<String, VariablesPointerAndTypes<'ctx>>,
    name: &str,
) -> PointerValue<'ctx> {
    variables
        .get(name)
        .unwrap_or_else(|| panic!("Undefined variable: {}", name))
        .ptr
}

fn expr_deref(srctype: &Expr) -> Expr {
    match srctype {
        Expr::TypeApply { base: _base, args } => args[0].clone(),
        _ => panic!("unexpected dereference"),
    }
}
