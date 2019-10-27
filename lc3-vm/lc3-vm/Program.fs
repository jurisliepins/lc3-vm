open lc3vm

[<EntryPoint>]
let main argv =
    try
        LC3VirtualMachine.Instance.ProgramCounter <- 0x3000us
        
        let lower, upper = LC3VirtualMachine.Instance.Load("C:\\Users\\Yuris Liepins\\Projects\\lc3-vm\\images\\2048.obj")
        let memory, _ = LC3VirtualMachine.Instance.VirtualMachine 
        
        LC3VirtualMemory.dump memory lower upper

        //LC3VirtualMachine.eval LC3VirtualMachine.Instance
        //(LC3Disassembler.disassembleVirtualMachine LC3VirtualMachine.Instance) |> LC3Disassembler.print
    with
        | ex -> printfn "%A" (ex.ToString())
    0