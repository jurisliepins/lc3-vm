open lc3vm

[<EntryPoint>]
let main argv =
    try
        LC3VirtualMachine.Instance.ProgramCounter <- 0x3000us

        LC3VirtualMachine.load LC3VirtualMachine.Instance "./2048.obj" |> ignore
        LC3VirtualMachine.eval LC3VirtualMachine.Instance
    with
        | ex -> printfn "%A" ex
    0