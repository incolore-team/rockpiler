use std::{
    io::{stdout, Write},
    vec,
};

use linked_hash_map::LinkedHashMap;

use crate::{
    cli_args::CliArgs,
    emit_llvm,
    ir::{
        self,
        module::Module,
        scope::ScopeCursor,
        sema::{self, SemaContext},
        ssa, ty::Ty,
    },
    parser,
};

/// ! The compiler driver is responsible for invoking the various phases of the
/// ! compiler.

pub struct Driver<'l_args> {
    args: &'l_args CliArgs,
}

impl<'lt_arg> Driver<'lt_arg> {
    pub fn new(args: &'lt_arg CliArgs) -> Driver<'lt_arg> {
        Driver { args }
    }

    pub fn run(&self) -> Result<(), String> {
        let parser = parser::PolygonParser::default();
        for input_filename in &self.args.input {
            let input = std::fs::read_to_string(input_filename).unwrap();

            // 1. Parse the source file
            let r = parser.parse_src(input.as_str());
            if let Err(e) = r {
                return Err(format!("Error parsing file {}: {}", input_filename, e));
            }
            let ast = r.unwrap();
            let mut m = Module::default();

            // 2. Initialize the built-in things
            init_builtin(&mut m);

            // 3. Semantic analysis
            sema::sema_analyze(&mut m, &ast);

            // 4. Build the IR
            let sr_clone = m.sr.clone();
            // --- create a scope cursor for the IR builder to visiting the scope tree
            let mut se = SemaContext::new(ScopeCursor::new(&sr_clone));
            ssa::build_ir(&mut m, &mut se, &ast);

            let base_name = std::path::Path::new(input_filename)
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            // 5. Emit LLVM IR
            let bulid_dir = "build";

            std::fs::create_dir_all(bulid_dir).unwrap();

            let ll_file = format!("{}/{}.ll", bulid_dir, base_name);
            let mut llvm_out = std::fs::File::create(ll_file.clone()).unwrap();
            emit_llvm::emit(&m, &mut llvm_out);

            // 6. Compile the LLVM IR to an executable

            let mut out = stdout();

            // clang -c FILE.ll -o FILE.o
            let obj_file = format!("{}/{}.o", bulid_dir, base_name);
            let mut cc_cmd = std::process::Command::new("clang");

            cc_cmd
                .arg("-c")
                .arg(ll_file)
                .arg("-o")
                .arg(obj_file.clone());

            log::debug!("Running cc: {:?}", cc_cmd);

            let cc = cc_cmd.output();

            match cc {
                Ok(cc) => {
                    if !cc.status.success() {
                        out.write_all("Error running cc:\n".as_bytes()).unwrap();
                        out.write_all(cc.stderr.as_slice()).unwrap();
                        return Err("failed".to_string());
                    }
                }
                Err(e) => {
                    out.write_all("Error running cc:\n".as_bytes()).unwrap();
                    out.write_all(e.to_string().as_bytes()).unwrap();
                    return Err("failed".to_string());
                }
            }
            // clang -o FILE FILE.o -L./lib -lllpoly -Wl
            let exe_file = format!("{}/{}", bulid_dir, base_name);
            let lib_path = "lib";
            let mut cc_cmd = std::process::Command::new("clang");
            let static_ = true;
            cc_cmd
                .arg("-o")
                .arg(exe_file)
                .arg(obj_file)
                .arg(format!("-L{}", lib_path).as_str())
                .arg("-lllpoly")
                .arg("-Wl");

            if static_ {
                cc_cmd.arg("-static");
            }

            log::debug!("Running cc: {:?}", cc_cmd);

            let cc = cc_cmd.output();

            match cc {
                Ok(cc) => {
                    if !cc.status.success() {
                        out.write_all("Error running cc:\n".as_bytes()).unwrap();
                        out.write_all(cc.stderr.as_slice()).unwrap();
                        return Err("failed".to_string());
                    }
                }
                Err(e) => {
                    out.write_all("Error running cc:\n".as_bytes()).unwrap();
                    out.write_all(e.to_string().as_bytes()).unwrap();
                    return Err("failed".to_string());
                }
            }
        }
        Ok(())
    }
}

fn init_builtin(ctx: &mut Module) {
    {
        let stdout = ir::value::Var {
            name: "stdout".to_string(),
            ty: Ty::ptr(),
            is_const: true,
            is_external: true,
            is_global: true,
            is_bultin: true,
            ..Default::default()
        };
        let vid = ctx.register_psuedo_var(stdout, "@stdout".to_string());
        let o = ctx.declare("stdout".to_string(), vid);
        assert!(o.is_none());
    }
    {
        let fd_name = "fd".to_string();
        let fd_var = ir::value::Var::new_param(fd_name.clone(), Ty::ptr());
        let fd_vid = ctx.register_psuedo_var(fd_var, "%0".to_string());

        let buf_name = "buf".to_string();
        let buf_var = ir::value::Var::new_param(buf_name.clone(), Ty::ptr());
        let buf_vid = ctx.register_psuedo_var(buf_var, "%1".to_string());

        let func_name = "write_str".to_string();
        let func_vid = ctx.register_func(ir::value::Func {
            name: func_name.clone(),
            is_external: true,
            is_builtin: true,
            bb_map: LinkedHashMap::new(),
            entry_bb: None,
            params: vec![fd_vid, buf_vid],
            ret_ty: Ty::int(32),
            ..Default::default()
        });
        ctx.declare(func_name, func_vid);
    }
}
