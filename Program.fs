open Parse
open Gen
open RustDef

[<EntryPoint>]
let main argv =
    printfn "%A" argv

    if (Array.length argv) <> 2 then
        failwith "This program expects arguments 'definition_file' and 'output_file'"

    //Parse.test_me ()

    let def = new System.IO.StreamReader(argv.[0])
    let txt = def.ReadToEnd()

    let ast = Parse.getAst txt
    let regs, asm =
        match ast with
        | Some(ast) ->
            Parse.printAst ast
            Gen.gen_structs ast
        | None ->
            failwith "could not generate structs"

    let rust_defs = rust_def regs asm
    System.IO.File.WriteAllText(argv.[1], rust_defs)
    //let f = new System.IO.FileStream(RUST_DIR+"x86.rs", System.IO.FileMode.Create)
    //f.Write(rust_defs)

    //printfn "res: %A" (System.Diagnostics.Process.Start("/bin/ls", "/bin"))
    System.Console.ReadLine |> ignore
    0 // return an integer exit code
