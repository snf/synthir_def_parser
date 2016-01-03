module Parse

open FParsec

// Ast definition
type RegTypes = GPR | VECTOR | FLAGS
type RegType = RegTypes list

type arg =
    | String of string
    | Number of int
    | Id of string
    | Flags of string list

type expr =
    | Call of string * arg list
    | Constant of string
    // Internal calls
    | Reg of string * int * RegType
    | SubReg of string * int * int * RegType

type stmt =
    | Assign of string * expr
    | Macro of string * arg list * stmt list
    | MacroCall of string * arg list
    | Comment of string

type Ast = stmt list

let reserved_words = ["macro"; "end"]

// Parser helpers
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = many (anyOf "\t ")
let str_ws s = ws >>. pstring s .>> ws

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    between (pstring "\"") (pstring "\"")
            (manyChars normalChar)

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    ws >>.
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace

let flags_or_id : Parser<arg, unit> =
    sepBy (identifier) (str_ws "|")
    |>>
    (fun l -> if (List.length l) > 1 then (Flags l) else (Id l.[0]))

let argument = choice [
                attempt stringLiteral |>> String
                attempt (pint32 |>> Number)
                attempt (flags_or_id)
               ]

// Parser properly
let call_args : Parser<arg list, unit> = str_ws "("  >>. sepBy argument (str_ws ",") .>> str_ws ")"
let call_expr : Parser<expr, unit> = pipe2 identifier call_args (fun name args -> Call(name, args))

let expr : Parser<expr, unit> = choice [
                                    attempt call_expr
                                    attempt identifier |>> Constant
                                ]

let statement, statementRef = createParserForwardedToRef<stmt, unit>()

let macro_args : Parser<arg list, unit> = str_ws "("  >>. sepBy argument (str_ws ",") .>> str_ws ")"
let macro_body : Parser<stmt list, unit> = many statement

let macro_def : Parser<stmt, unit> =
    between (str_ws "macro") (str_ws "end") (
        pipe3 identifier (macro_args .>> newline) (macro_body) (fun id args body -> Macro(id, args, body)))

let stmt_assign : Parser<stmt, unit> = pipe3 identifier (str_ws "=") expr (fun id _ e -> Assign(id, e))

let macro_call: Parser<stmt, unit> = pipe2 identifier call_args (fun name args -> MacroCall(name, args))

let line_comment: Parser<stmt, unit> = (str_ws "//") >>. restOfLine true |>> Comment

do statementRef := choice [
                    attempt stmt_assign
                    attempt macro_def
                    attempt macro_call
                    attempt line_comment
                   ] .>> skipMany newline

let definition = many statement

//type Macro = { arg
type MacroMap = Map<string, arg>
let get_macros (ast: Ast): stmt list =
    List.fold (
        fun x s ->
            match s with
            | Macro(_, _, _) -> s :: x
            | _ -> x
    ) [] ast

type ReplaceMap = Map<string, arg>
let create_replace args m_args : ReplaceMap =
    List.fold2 (fun m a m_a ->
                    match m_a with
                    | Id id -> Map.add id a m
                    | _ -> failwith "only supports Id as macro argument"
    ) Map.empty args m_args

let replace_s r_list s =
    match Map.tryFind s r_list with
    | Some(new_e) -> match new_e with
                     | Id(new_s) -> new_s
                     | _ -> failwith "not expected"
    | None -> s

let replace_a r_list a =
    match a with
    | Id id -> Id (replace_s r_list id)
    | _ -> a

let replace_e r_list e =
    match e with
    | Call(name, args) -> Call(name, List.map (fun a -> replace_a r_list a) args)
    | _ -> e

let replace_in_stmt (r_list: ReplaceMap) stmt =
    match stmt with
    | Assign(a, e) -> Assign(replace_s r_list a, replace_e r_list e)
    | MacroCall(name, args) -> MacroCall(name, List.map (fun a -> replace_a r_list a) args)
    | Macro(_, _, _) -> failwith "macro should not be replaced with other macro"
    | _ -> stmt

let replace_macros (ast: Ast): Ast =
    let macros = get_macros ast
    let macro_find name =
        List.tryFind (fun s ->
            match s with
            | Macro(n, _, _) when n = name-> true
            | _ -> false
        ) ast
    let replace_macro s =
        match s with
        | MacroCall(name, args) ->
            match (macro_find name) with
            | Some(Macro(_, m_args, m_stmts)) ->
                if List.length m_args <> List.length args then failwith (sprintf "macro call: %s has wrong number of args" name)
                else
                    List.map (fun stmt -> replace_in_stmt (create_replace args m_args) stmt) m_stmts
            | None -> [s]
            | _ -> failwith "macro not found, error?"
        | _ -> [s]
    List.map (fun s -> replace_macro s) ast
    // Flatten list
    |> List.concat
    // Filter all the macro definitions once they were processed and comments
    |> List.filter (fun stmt ->
        match stmt with
        | Macro(_, _, _) | Comment(_) -> false
        | _ -> true
    )

let process_expr e =
    match e with
    | Call(name, args) when name = "Bit" -> Call("SubReg", [args.[0]; Number 1; args.[1]])
    | Call(name, args) when name = "Bits" ->
        let bits = List.length args - 1
        Call("SubReg", [args.[0]; Number bits; args.[1]])
    | _ -> e

let replace_funcs (ast: Ast): Ast =
    ast
    |> List.map (fun stmt ->
        match stmt with
        | Assign(name, expr) -> Assign(name, process_expr expr)
        | _ -> stmt
    )


let printAst (ast: Ast) =
    ast
    |> List.iter (fun s -> printfn "%A" s)

let getAst text =
    match run definition text with
    | Success(res, _, _) ->
        Some(
            res
            |> replace_macros
            |> replace_funcs
        )
    | Failure(err, _, _) -> printfn "Err: %A" err; None


let test_me () =
    test macro_def """macro GPR_def(rq, rd)
    rq = Reg(10, GPR)
    end"""
    test macro_args "(a, b, c, d)"
    test macro_body """rq = Reg(64, GPR, VEC | CC)
    ra = SubReg(rq, 0x20, 0)"""
    test definition """macro GPR_def(rq, rd)
    rq = Reg(10, GPR)

    end

    GPR_def(RAX, EAX)
    """
