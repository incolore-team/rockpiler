use std::io::Write;

use crate::{
    ir::{self, ConstVal, ModuleContext, TypedValue, ValueId, ValueProto},
    value_cast,
};

type M = ModuleContext;

pub fn emit(m: &M, output: &mut dyn Write) {
    let global_vals: Vec<ValueId> = m.global_val_map.values().copied().collect();
    for global_val in global_vals {
        emit_global_val(m, global_val, output);
    }

    let const_strs: Vec<(ValueId, String)> = m
        .const_str_map
        .iter()
        .map(|(k, v)| (*v, k.clone()))
        .collect();
    for (id, s) in const_strs {
        emit_const_str(m, id, s, output);
    }

    let funcs: Vec<ValueId> = m.funcs_map.values().copied().collect();
    for func_id in funcs {
        emit_func(m, func_id, output);
    }
}

fn emit_global_val(ctx: &M, id: ValueId, output: &mut dyn Write) {
    let global_val_val = ctx.resolve_value(id);
    let global_val = value_cast!(global_val_val, ValueProto::GlobalVal(ref v)=> v);
    writeln!(
        output,
        "{name} = {linkage} global {ty} {init}, align {align}",
        name = global_val_val.id_name,
        linkage = match global_val.is_external {
            true => "external",
            false => "internal",
        },
        ty = emit_ty(&global_val.ty),
        init = match global_val.init {
            Some(init) => {
                // TODO: may fail here
                let const_val = ctx.resolve_const_val(init);
                format_const_val(ctx, const_val, false)
            }
            None => "".to_string(),
        },
        align = emit_align(&global_val.ty),
    )
    .unwrap();
}

fn emit_align(ty: &ir::Ty) -> usize {
    match ty {
        ir::Ty::Void => todo!(),
        ir::Ty::Int(i) => i.bit_width / 8,
        ir::Ty::Float(_) => todo!(),
        ir::Ty::Ptr(_) => 8,
        ir::Ty::Array(_) => todo!(),
        ir::Ty::Func(_) => todo!(),
    }
}

fn emit_const_str(ctx: &M, id: ValueId, s: String, output: &mut dyn Write) {
    let const_str_value = ctx.resolve_value(id);
    writeln!(
        output,
        "{sym_name} = private unnamed_addr constant [{len} x i8] c\"{s}\\00\"",
        sym_name = const_str_value.id_name,
        len = s.len() + 1,
        s = s
    )
    .unwrap();
}

fn format_const_val(_ctx: &M, const_val: &ConstVal, with_ty: bool) -> String {
    match const_val {
        ir::ConstVal::None => unreachable!("const val should not be none"),
        ir::ConstVal::Int(iv) => {
            format!(
                "{ty} {val}",
                ty = {
                    if with_ty {
                        emit_ty(&iv.ty)
                    } else {
                        "".to_string()
                    }
                },
                val = iv.val
            )
        }
        ir::ConstVal::Float(_fv) => todo!(),
    }
}

fn emit_func(ctx: &M, id: ValueId, output: &mut dyn Write) {
    let func_value = ctx.resolve_value(id);
    let func = ctx.resolve_func(id);
    if func.is_external {
        // writeln!(output, "declare {} @{}(", emit_ty(&func.ret_ty), func.name).unwrap();
        // for param in &func.params {
        //     writeln!(output, "    {} %{}", emit_ty(&param.ty), param.name).unwrap();
        // }
        // writeln!(output, ")").unwrap();

        writeln!(
            output,
            "declare {ty} {name} ({params})",
            ty = emit_ty(&func.ret_ty),
            name = func_value.id_name,
            params = func
                .params
                .iter()
                .map(|p| emit_ty(&p.ty))
                .collect::<Vec<_>>()
                .join(", ")
        )
        .unwrap();
        return;
    }
    // writeln!(output, "define {} @{}(", emit_ty(&func.ret_ty), func.name).unwrap();
    // for param in &func.params {
    //     writeln!(output, "    {} %{}", emit_ty(&param.ty), param.name).unwrap();
    // }
    // writeln!(output, ") {{").unwrap();
    // let bb_ids = func.bb_map.values().copied().collect::<Vec<_>>();
    // for bb_id in bb_ids {
    //     emit_bb(ctx, bb_id, output);
    // }
    // writeln!(output, "}}").unwrap();
    writeln!(
        output,
        "define {ty} {name} ({params}) {{",
        ty = emit_ty(&func.ret_ty),
        name = emit_func_name(func.name.as_str()),
        params = func
            .params
            .iter()
            .map(|p| format!("{} {}", emit_ty(&p.ty), p.id_name))
            .collect::<Vec<_>>()
            .join(", ")
    )
    .unwrap();
    let bb_ids = func.bb_map.values().copied().collect::<Vec<_>>();
    for bb_id in bb_ids {
        emit_bb(ctx, bb_id, output);
    }
    writeln!(output, "}}").unwrap();
}

fn emit_func_name(name: &str) -> String {
    format!("@{}", name)
}

fn emit_ty(ty: &ir::Ty) -> String {
    match ty {
        ir::Ty::Void => todo!(),
        ir::Ty::Int(i) => {
            format!("i{}", i.bit_width)
        }
        ir::Ty::Float(_) => todo!(),
        ir::Ty::Ptr(_) => "ptr".to_string(),
        ir::Ty::Array(_) => todo!(),
        ir::Ty::Func(_) => todo!(),
    }
}

fn emit_bb(ctx: &M, id: ValueId, output: &mut dyn Write) {
    let bb = ctx.resolve_bb(id);
    writeln!(output, "  {label}:", label = bb.name).unwrap();
    let inst_ids = bb.insts.to_vec();
    for inst_id in inst_ids {
        emit_inst(ctx, inst_id, output);
    }
}

fn emit_inst(ctx: &M, inst_id: ValueId, output: &mut dyn Write) {
    let inst = ctx.resolve_inst(inst_id);
    match inst {
        ir::Inst::None => unreachable!(),
        ir::Inst::Call(call_inst) => emit_call_inst(ctx, inst_id, call_inst, output),
        ir::Inst::Ret(ret_inst) => emit_ret_inst(ctx, inst_id, ret_inst, output),
        ir::Inst::Load(load_inst) => emit_load_inst(ctx, inst_id, load_inst, output),
    }
}

fn emit_call_inst(ctx: &M, inst_id: ValueId, call_inst: &ir::CallInst, output: &mut dyn Write) {
    let inst_val = ctx.resolve_value(inst_id);
    let callee_func_val = ctx.resolve_value(call_inst.func);
    let callee_func = ctx.resolve_func(call_inst.func);
    let ret_ty = emit_ty(&callee_func.ret_ty);
    let arg_ids = call_inst.args.to_vec();
    let mut arg_items = Vec::new();
    for arg_id in arg_ids {
        let arg_val = ctx.resolve_value(arg_id);
        let arg_name = arg_val.id_name.clone();
        let arg_item = format!(
            "{ty} {name}",
            ty = emit_ty(&arg_val.ty(ctx)),
            name = arg_name
        );
        arg_items.push(arg_item);
    }
    let args = arg_items.join(", ");
    writeln!(
        output,
        "    {id_name} = call {ret_ty} {func_name}({args})",
        id_name = inst_val.id_name,
        ret_ty = ret_ty,
        func_name = callee_func_val.id_name,
        args = args
    )
    .unwrap();
}

fn emit_ret_inst(ctx: &M, _inst_id: ValueId, ret_inst: &ir::RetInst, output: &mut dyn Write) {
    let ret_val = ctx.resolve_value(ret_inst.val.unwrap());
    let ret_ty = emit_ty(&ret_val.ty(ctx));
    writeln!(
        output,
        "    ret {ret_ty} {ret_val}",
        ret_ty = ret_ty,
        ret_val = format_val(ctx, ret_val)
    )
    .unwrap();
}

fn emit_load_inst(ctx: &M, inst_id: ValueId, load_inst: &ir::LoadInst, output: &mut dyn Write) {
    let inst_val = ctx.resolve_value(inst_id);
    let ptr_val = ctx.resolve_value(load_inst.ptr);
    let load_ty = emit_ty(&ptr_val.ty(ctx));
    writeln!(
        output,
        "    {id_name} = load {load_ty}, ptr {ptr_id_name}, align 4",
        id_name = inst_val.id_name,
        load_ty = load_ty,
        ptr_id_name = ptr_val.id_name,
    )
    .unwrap();
}

fn format_val(ctx: &M, val: &ir::Value) -> String {
    match &val.proto {
        ValueProto::ConstVal(cv) => format_const_val(ctx, cv, false),
        ValueProto::None => todo!(),
        ValueProto::GlobalVal(_gv) => todo!(),
        ValueProto::ConstStr(_) => todo!(),
        ValueProto::Func(_) => todo!(),
        ValueProto::BasicBlock(_) => todo!(),
        ValueProto::Inst(_) => todo!(),
    }
}
