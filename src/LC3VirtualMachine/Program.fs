open LC3VirtualMachine

let parseCommandLineArgs (args: string[]) = if (args.Length < 1) then None else Some (args |> Array.toList)

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
        match parseCommandLineArgs argv with
        | Some (command::commandOptions) ->
            match (command::commandOptions) with
            | ("-h"::_)    | ("--help"::_) -> printUsage ()
            | ("-p"::path) | ("--path"::path) ->
                let vm = LC3VirtualMachine()
                vm.ProgramCounter <- 0x3000us
                
                LC3VirtualMachine.load vm (List.head path) |> ignore
                LC3VirtualMachine.eval vm
            | _ -> 
                printUsage ()
        | Some ([])
        | None ->
            printUsage ()
    with
        | ex -> printfn "%A" ex
    0