module Gen

open Parse

type GAst =
    | C

type RegType =
    | GPR       // Can be anything
    | Vector    // Can have many values packed in the register
    | Flag      // The register doesn't mean anything, only the subregs should be checked
    | Segment   // Change the address type
    | Fp32
    | Fp64
    | Fp80
    | IP

type SubReg = {
    name: string;
    from_bit: int;
    width: int;
    }

type Reg = {
    name: string;
    group: string;
    ty: RegType list;
    width: int;
    store_offset: int;
    sub_regs: SubReg list;
    }

type State = {
    regs: Reg list;
    curr_offset: int;
    base_reg: string option;
    }

let interior_macros = ["STORE", "LOAD"]

let get_id arg =
    match arg with
    | Id(s) -> s
    | _ -> failwith (sprintf "expecting Id but found: %A" arg)

let get_string arg =
    match arg with
    | String(s) -> s
    | _ -> failwith (sprintf "expecting Id but found: %A" arg)

let get_number arg =
    match arg with
    | Number(i) -> i
    | _ -> failwith (sprintf "expecting number but got: %A" arg)

let string_to_flag s =
    match s with
    | "VECTOR" -> Vector
    | "FP80"   -> Fp80
    | "FP32"   -> Fp32
    | "FP64"   -> Fp64
    | "GPR"    -> GPR
    | "FLAGS"  -> Flag
    | "IP"     -> IP
    | _        -> failwith (sprintf "flag not recognized: %s" s)


let get_flags arg =
    match arg with
    | Flags(l) -> l |> List.map string_to_flag
    | Id(s)    -> [ (string_to_flag s) ]
    | _ -> failwith "not flags"

let find_reg state name =
    state.regs
    |> List.tryFind (fun x -> x.name = name)
    |> fun x -> match x with
                | Some(r) -> r
                | None -> failwith (sprintf "register %s is not defined" name)

let process_reg state name args =
    assert ((List.length args) >= 2)
    let width, group = (get_number args.[0]), (get_id args.[1])
    let ty = if (List.length args) = 3 then (get_flags args.[2]) else []
    // Align to it's width to avoid any alignement problem
    let byte_width = width / 8
    let this_offset' = state.curr_offset &&& (~~~(byte_width-1))
    let this_offset  = if this_offset' = state.curr_offset then this_offset' else this_offset' + byte_width
    let reg = { name=name; group=group; ty=ty; width=width;
                store_offset=this_offset; sub_regs=[] }
    { state with
        regs = reg :: state.regs;
        curr_offset = this_offset + byte_width }

let process_sub_reg state name args =
    assert((List.length args) = 3)
    let base_reg_name, width, from_bit = (get_id args.[0]), (get_number args.[1]), (get_number args.[2])
    let base_reg = find_reg state base_reg_name
    let new_reg = { name=name; from_bit=from_bit; width=width }
    let nregs =
        state.regs
        |> List.map (fun r ->
            if r.name = base_reg_name then
                { base_reg with
                    sub_regs = new_reg :: r.sub_regs }
            else
                r
        )
    { state with
        regs = nregs }

let process_base_reg state reg =
    let r = find_reg state reg
    { state with
        base_reg = Some(reg)}

// Process the ast and generate the reg structs
let get_regs (ast: Parse.Ast) =
    let mutable last_off = 0
    ast
    |> List.fold (
        fun s stmt ->
            match stmt with
            | Assign("BaseReg", Constant(reg)) -> process_base_reg s reg
            | Assign(name, expr) ->
                match expr with
                | Call(f_name, args) when f_name = "Reg" -> process_reg s name args
                | Call(f_name, args) when f_name = "SubReg" -> process_sub_reg s name args
                | _ -> failwith (sprintf "name not recognized: %s" name)
            | MacroCall(name, args) when name = "LOAD" -> s
            | MacroCall(name, args) when name = "STORE" -> s
            | _ -> failwith (sprintf "error: %A" stmt)
    ) {regs = []; curr_offset = 0; base_reg = None}

let find_x_by_id ast id x =
    ast
    |> List.tryPick (
        fun stmt ->
            match stmt with
            | MacroCall(name, args) when name = x ->
                let l_id, l_def = get_id args.[0], get_string args.[1]
                let l_priority = if (List.length args) = 3 then get_number args.[2] else 5
                if id = l_id then Some(l_def, l_priority) else None
            | _ -> None
    )

let find_load_by_id ast id = find_x_by_id ast id "LOAD"
let find_store_by_id ast id= find_x_by_id ast id "STORE"

let find_x ast reg x =
    // First try to search by reg_name
    match find_x_by_id ast reg.name x with
    | Some(def) -> def
    | None ->
        // Then try to search by group
        match find_x_by_id ast reg.group x with
        | Some(def) -> def
        | None -> failwith (sprintf "can not find def for reg: %A" reg)
    |> (fun (s, p) -> s.Replace("%1", reg.name).Replace("%2", string reg.store_offset), p)
    // Here I should add a counter for knowing how many steps it has to make till it hits the
    // real instruction.
    //|> List.map (fun (s, p) -> 

let find_load ast reg = find_x ast reg "LOAD"
let find_store ast reg = find_x ast reg "STORE"

let gen_asm_code ast regs =
    // The only requirement is loading the BaseReg at the start and the BaseReg register at the end
    //let basereg_name = Option.get (regs.base_reg)
    let basereg_load = Option.get (find_load_by_id ast "BASE_REG")
    let basereg_store = Option.get (find_store_by_id ast "BASE_REG")
    let sort_and_clean r = r |> List.sortBy (fun (r, p) -> p) |> List.map (fun (r, p) -> r)
    let regs_load = basereg_load :: (regs.regs |> List.map (fun r -> find_load ast r))   |> sort_and_clean
    let regs_store= basereg_store ::(regs.regs |> List.map (fun r -> find_store ast r))  |> sort_and_clean
    regs_load, regs_store
    //printfn "%A" regs_load

let gen_structs (ast: Parse.Ast) =
    let regs = get_regs ast
    let asm = gen_asm_code ast regs
    printfn "%A" regs
    printfn "%A" asm
    regs, asm

let test_me ast =
    gen_structs ast

