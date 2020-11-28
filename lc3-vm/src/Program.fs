open lc3_vm

let parseCommandLinePath (args: string[]) = if (args.Length < 1) then None else Some args.[0]

let printUsage () = 
    printfn "usage: lc3-vm [--help] [<path>]"
    printfn "example: lc3-vm ./2048.obj"

[<EntryPoint>]
let main argv =
    try
        match parseCommandLinePath argv with
        | Some arg ->
            match arg with
            | "--help" -> 
                printUsage ()
            | path -> 
                LC3VirtualMachine.Instance.ProgramCounter <- 0x3000us
                
                LC3VirtualMachine.load LC3VirtualMachine.Instance path |> ignore
                LC3VirtualMachine.eval LC3VirtualMachine.Instance
        | None ->
            printUsage ()
    with
        | ex -> printfn "%A" ex
    0