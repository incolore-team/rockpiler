use std::io::{stdout, Write};

use linked_hash_map::LinkedHashMap;

use crate::{
    cli_args::CliArgs,
    emit_llvm,
    ir::{self, ModuleContext},
    parser,
};

/// ! The compiler driver is responsible for invoking the various phases of the
/// ! compiler.

pub struct Driver<'lt_args> {
    args: &'lt_args CliArgs,
}

impl<'lt_arg> Driver<'lt_arg> {
    pub fn new(args: &'lt_arg CliArgs) -> Driver<'lt_arg> {
        Driver { args }
    }

    pub fn run(&self) -> Result<(), String> {
        let parser = parser::PolygonParser::default();
        for input_filename in &self.args.input {
            let input = std::fs::read_to_string(input_filename).unwrap();
            let r = parser.parse_src(input.as_str());
            if let Err(e) = r {
                return Err(format!("Error parsing file {}: {}", input_filename, e));
            }
            let ast = r.unwrap();
            let mut m = ModuleContext::default();
            init_builtin(&mut m);
            ir::build_ir(&mut m, &ast);

            let base_name = std::path::Path::new(input_filename)
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();

            let bulid_dir = "build";

            std::fs::create_dir_all(bulid_dir).unwrap();

            let ll_file = format!("{}/{}.ll", bulid_dir, base_name);
            let mut llvm_out = std::fs::File::create(ll_file.clone()).unwrap();
            emit_llvm::emit(&m, &mut llvm_out);

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
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    out.write_all("Error running cc:\n".as_bytes()).unwrap();
                    out.write_all(e.to_string().as_bytes()).unwrap();
                    std::process::exit(1);
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
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    out.write_all("Error running cc:\n".as_bytes()).unwrap();
                    out.write_all(e.to_string().as_bytes()).unwrap();
                    std::process::exit(1);
                }
            }
        }
        Ok(())
    }
}

fn init_builtin(ctx: &mut ModuleContext) {
    {
        let stdout = ir::GlobalVal {
            name: "stdout".to_string(),
            ty: ir::Ty::int(32),
            is_const: true,
            is_external: true,
            is_bultin: true,
            ..Default::default()
        };
        ctx.register_global_val(stdout);
    }
    ctx.register_func(ir::Func {
        name: "write_str".to_string(),
        is_external: true,
        is_builtin: true,
        bb_map: LinkedHashMap::new(),
        entry_bb: None,
        params: vec![
            ir::Param {
                name: "fd".to_string(),
                ty: ir::Ty::ptr(),
                id_name: "%1".to_string(),
            },
            ir::Param {
                name: "buf".to_string(),
                ty: ir::Ty::ptr(),
                id_name: "%2".to_string(),
            },
        ],
        ret_ty: ir::Ty::int(32),
    });
}
