namespace LC3VirtualMachine

open System.IO

module Program =
    let parseCommandLineArgs (args: string[]): string list option =
        if (args.Length < 1) then
            None
        else
            Some(args |> Array.toList)

    let printUsage (): unit = 
        printfn "Usage: lc3-vm [command] [command-option]"
        printfn ""
        printfn "Commands:"
        printfn "   -h | --help     Display this help menu."
        printfn "   -p | --path     Path to input .obj file to run."
        printfn ""

    [<EntryPoint>]
    let main (argv: string []): int =
        match parseCommandLineArgs argv with
        | Some(command::commandOptions) ->
            match (command::commandOptions) with
            | "-h"::_       | "--help"::_ -> printUsage ()
            | "-p"::path::_ | "--path"::path::_ ->
                try
                    let vm = VirtualMachine(0x3000us)
                    VirtualEvaluator.load vm
                            (new BinaryReader(
                                File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
                            ) |> ignore
                    VirtualEvaluator.eval vm 
                with ex -> printfn $"%A{ex}"
            | _ -> printUsage ()
        | _ -> printUsage ()
        0