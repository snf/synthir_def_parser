module RustDef

open Gen

let rust_code = """
use std::collections::HashMap;

use definitions::{RegDefinition, SubRegDefinition, Definition, GenDefinition, Endianness};

//pub struct X86;

impl GenDefinition for X86_64 {
    fn gen_definition() -> Definition {
        let mut regs = HashMap::new();
        let mut sub_regs = HashMap::new();
        REGISTERS
        Definition {
            base_reg: "BASE_REG",
            regs_size: REGS_SIZE,
            regs: regs,
            sub_regs: sub_regs,
            epilogue: "EPILOGUE",
            prologue: "PROLOGUE",
            epilogue_steps: EPILOGUE_STEPS,
            prologue_steps: PROLOGUE_STEPS,
            endian: Endianness::Little
        }
    }
}
"""

let reg_definition = """
    regs.insert("REG_NAME", RegDefinition {
        offset: REG_OFF, width: REG_WIDTH, fp: REG_FP, ip: REG_IP,
        vector: REG_VECTOR, segment: REG_SEGMENT, flags: REG_FLAG,
        sp: REG_SP,
        sub_regs: vec![SUB_REGS]
    });
    """

let subreg_definition = """
    sub_regs.insert("REG_NAME", SubRegDefinition {
        parent: "REG_PARENT", from_bit: FROM_BIT,
        width: REG_WIDTH
    });
"""

let rust_def state asm =
    let regs = state.regs
    let regs_text =
        regs
        |> List.map (fun r ->
            let name = r.name
            let off = r.store_offset
            let width = r.width
            let bool2str x = if x = true then "true" else "false"
            let is_fp = r.ty |> List.exists (fun x -> match x with | Fp32 | Fp64 | Fp80 -> true | _ -> false) |> bool2str
            let is_vec= r.ty |> List.exists (fun x -> x = Vector) |> bool2str
            let is_sp = r.ty |> List.exists (fun x -> x = SP) |> bool2str
            let is_ip = r.ty |> List.exists (fun x -> x = IP) |> bool2str
            let is_seg= r.ty |> List.exists (fun x -> x = Segment) |> bool2str
            let is_flag=r.ty |> List.exists (fun x -> x = Flag) |> bool2str
            let sub_regs_names = r.sub_regs |> List.map (fun r -> "\"" + r.name + "\"") |> String.concat ","
            let t_reg = reg_definition.Replace("REG_NAME", name).Replace("REG_OFF", string off).Replace("REG_WIDTH", string width)
            let t_reg = t_reg.Replace("REG_FLAG", is_flag).Replace("REG_IP", is_ip).Replace("REG_VECTOR", is_vec)
            let t_reg = t_reg.Replace("REG_SEGMENT", is_seg).Replace("REG_FP", is_fp).Replace("REG_SP", is_sp)
            let t_reg = t_reg.Replace("SUB_REGS", sub_regs_names)
            let sub_regs =
                r.sub_regs
                |> List.map (fun sr ->
                    let sr_name = sr.name
                    let sr_from = sr.from_bit
                    let sr_width = sr.width
                    let sr_reg = subreg_definition.Replace("REG_NAME", sr_name).Replace("FROM_BIT", string sr_from)
                    let sr_reg = sr_reg.Replace("REG_WIDTH", string sr_width).Replace("REG_PARENT", name)
                    sr_reg
                )
            t_reg :: sub_regs
        )
        |> List.concat
        |> String.concat "\n"
    let final_code = rust_code.Replace("REGISTERS", regs_text).Replace("BASE_REG", state.base_reg.Value).Replace("REGS_SIZE", string state.curr_offset)
    let epilogue, epilogue_steps =
        snd asm
        |> List.filter (fun x -> x <> "PTRACE")
        |> List.fold (fun (text, num) ins ->
            (text + "\n" + ins, num + (Seq.length (ins.Split(';'))))
        ) ("", 0)
    let prologue, prologue_steps =
        fst asm
        |> List.filter (fun x -> x <> "PTRACE")
        |> List.fold (fun (text, num) ins ->
            (text + "\n" + ins, num + (Seq.length (ins.Split(';'))))
        ) ("", 0)
    let final_code = final_code.Replace("EPILOGUE_STEPS", string epilogue_steps).Replace("EPILOGUE", epilogue)
                                .Replace("PROLOGUE_STEPS", string prologue_steps).Replace("PROLOGUE", prologue)
    printfn "%A" final_code
    final_code
    //|> List.iter (fun a -> printfn "%A" a)
