open LC3VirtualMachine

let parseCommandLinePath (args: string[]) = if (args.Length < 1) then None else Some (args |> Array.toList)

let printUsage () = 
    printfn "Usage: lc3-vm [command] [command-option]"
    printfn ""
    printfn "Commands:"
    printfn "   -h | --help     Display this help menu."
    printfn "   -p | --path     Path to input .obj file to run."
    printfn ""

[<EntryPoint>]
let main argv =
    try
        match parseCommandLinePath argv with
        | Some (command::commandOptions) ->
            match (command::commandOptions) with
            | ("-h"::_) | ("--help"::_) -> printUsage ()
            | ("-p"::path) | ("--path"::path) -> 
                LC3VirtualMachine.Instance.ProgramCounter <- 0x3000us
                
                LC3VirtualMachine.load LC3VirtualMachine.Instance (List.head path) |> ignore
                LC3VirtualMachine.eval LC3VirtualMachine.Instance
            | _ -> 
                printUsage ()
        | Some ([])
        | None ->
            printUsage ()
    with
        | ex -> printfn "%A" ex
    0