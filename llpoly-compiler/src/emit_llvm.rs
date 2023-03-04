use std::io::Write;

use crate::{
    ir::{module::Module, ty::Ty, value::*},
    value_cast,
};

pub fn emit(m: &Module, output: &mut dyn Write) {
    let global_vals: Vec<ValueId> = m.global_var_map.values().copied().collect();
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

    let funcs: Vec<ValueId> = m.func_map.values().copied().collect();
    for func_id in funcs {
        emit_func(m, func_id, output);
    }
}

fn emit_global_val(ctx: &Module, id: ValueId, output: &mut dyn Write) {
    let global_vid = ctx.get_value(id);
    let global_val = value_cast!(global_vid, ValueProto::Var(ref v)=> v);
    writeln!(
        output,
        "{name} = {linkage} global {ty} {init}, align {align}",
        name = global_vid.display_name,
        linkage = match global_val.is_external {
            true => "external",
            false => "internal",
        },
        ty = emit_ty(&global_val.ty),
        init = match global_val.init {
            Some(init) => {
                // TODO: may fail here
                let imm_val = ctx.get_imm_val(init);
                format_imm_val(ctx, imm_val, false)
            }
            None => "".to_string(),
        },
        align = emit_align(&global_val.ty),
    )
    .unwrap();
}

fn emit_align(ty: &Ty) -> usize {
    match ty {
        Ty::Void => todo!(),
        Ty::Int(i) => i.bit_width / 8,
        Ty::Float(_) => todo!(),
        Ty::Ptr(_) => 8,
        Ty::Array(_) => todo!(),
        Ty::Func(_) => todo!(),
    }
}

fn emit_const_str(ctx: &Module, id: ValueId, s: String, output: &mut dyn Write) {
    let const_str_value = ctx.get_value(id);
    writeln!(
        output,
        "{sym_name} = private unnamed_addr constant [{len} x i8] c\"{s}\\00\"",
        sym_name = const_str_value.display_name,
        len = s.len() + 1,
        s = s
    )
    .unwrap();
}

fn format_imm_val(_ctx: &Module, imm_val: &ImmVal, with_ty: bool) -> String {
    match imm_val {
        ImmVal::None => unreachable!("const val should not be none"),
        ImmVal::Int(iv) => {
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
        ImmVal::Float(_fv) => todo!(),
    }
}

fn emit_func(ctx: &Module, id: ValueId, output: &mut dyn Write) {
    let func_value = ctx.get_value(id);
    let func = ctx.get_func(id);
    if func.is_external {
        writeln!(
            output,
            "declare {ty} {name} ({params})",
            ty = emit_ty(&func.ret_ty),
            name = func_value.display_name,
            params = func
                .params
                .iter()
                .map(|param_vid| {
                    let param_val = ctx.get_value(*param_vid);
                    let param = value_cast!(param_val, ValueProto::Var(ref v) => v);
                    format!("{} {}", emit_ty(&param.ty), param_val.display_name)
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
        .unwrap();
        return;
    }
    writeln!(
        output,
        "define {ty} {name} ({params}) {{",
        ty = emit_ty(&func.ret_ty),
        name = emit_func_name(func.name.as_str()),
        params = func
            .params
            .iter()
            .map(|param_vid| {
                let param_val = ctx.get_value(*param_vid);
                let param = value_cast!(param_val, ValueProto::Var(ref v) => v);
                format!("{} {}", emit_ty(&param.ty), param_val.display_name)
            })
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

fn emit_ty(ty: &Ty) -> String {
    match ty {
        Ty::Void => todo!(),
        Ty::Int(i) => {
            format!("i{}", i.bit_width)
        }
        Ty::Float(_) => todo!(),
        Ty::Ptr(_) => "ptr".to_string(),
        Ty::Array(_) => todo!(),
        Ty::Func(_) => todo!(),
    }
}

fn emit_bb(ctx: &Module, id: ValueId, output: &mut dyn Write) {
    let bb = ctx.get_bb(id);
    writeln!(output, "  {label}:", label = bb.name).unwrap();
    let inst_ids = bb.insts.to_vec();
    for inst_id in inst_ids {
        emit_inst(ctx, inst_id, output);
    }
}

fn emit_inst(ctx: &Module, inst_id: ValueId, output: &mut dyn Write) {
    let inst = ctx.get_inst(inst_id);
    match inst {
        Inst::None => unreachable!(),
        Inst::Call(call_inst) => emit_call_inst(ctx, inst_id, call_inst, output),
        Inst::Ret(ret_inst) => emit_ret_inst(ctx, inst_id, ret_inst, output),
        Inst::Load(load_inst) => emit_load_inst(ctx, inst_id, load_inst, output),
    }
}

fn emit_call_inst(ctx: &Module, inst_id: ValueId, call_inst: &CallInst, output: &mut dyn Write) {
    let inst_val = ctx.get_value(inst_id);
    let callee_func_val = ctx.get_value(call_inst.func);
    let callee_func = ctx.get_func(call_inst.func);
    let ret_ty = emit_ty(&callee_func.ret_ty);
    let arg_ids = call_inst.args.to_vec();
    let mut arg_items = Vec::new();
    for arg_id in arg_ids {
        let arg_val = ctx.get_value(arg_id);
        let arg_name = arg_val.display_name.clone();
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
        "    {display_name} = call {ret_ty} {func_name}({args})",
        display_name = inst_val.display_name,
        ret_ty = ret_ty,
        func_name = callee_func_val.display_name,
        args = args
    )
    .unwrap();
}

fn emit_ret_inst(ctx: &Module, _inst_id: ValueId, ret_inst: &RetInst, output: &mut dyn Write) {
    let ret_val = ctx.get_value(ret_inst.val.unwrap());
    let ret_ty = emit_ty(&ret_val.ty(ctx));
    writeln!(
        output,
        "    ret {ret_ty} {ret_val}",
        ret_ty = ret_ty,
        ret_val = format_val(ctx, ret_val)
    )
    .unwrap();
}

fn emit_load_inst(ctx: &Module, inst_id: ValueId, load_inst: &LoadInst, output: &mut dyn Write) {
    let inst_val = ctx.get_value(inst_id);
    let ptr_val = ctx.get_value(load_inst.ptr);
    let load_ty = emit_ty(&ptr_val.ty(ctx));
    writeln!(
        output,
        "    {display_name} = load {load_ty}, ptr {ptr_display_name}, align 4",
        display_name = inst_val.display_name,
        load_ty = load_ty,
        ptr_display_name = ptr_val.display_name,
    )
    .unwrap();
}

fn format_val(ctx: &Module, val: &Value) -> String {
    match &val.proto {
        ValueProto::ImmVal(cv) => format_imm_val(ctx, cv, false),
        ValueProto::None => todo!(),
        ValueProto::Var(_gv) => todo!(),
        ValueProto::ConstStr(_) => todo!(),
        ValueProto::Func(_) => todo!(),
        ValueProto::BasicBlock(_) => todo!(),
        ValueProto::Inst(_) => todo!(),
    }
}
