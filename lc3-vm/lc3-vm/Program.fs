open lc3vm
open System

[<EntryPoint>]
let main argv =
    let vm =  
        LC3VirtualMachineTypes.VirtualMachine(
            Array.zeroCreate (int UInt16.MaxValue), 
            Array.zeroCreate (int LC3VirtualMachineTypes.RegisterTypes.R_COUNT))

    try
        LC3VirtualMachine.writeProgramCounter vm 0x3000us
        LC3VirtualMachine.load vm "C:\\Users\\Yuris Liepins\\Projects\\lc3-vm\\images\\2048.obj" |> ignore
        LC3VirtualMachine.eval vm
    with
        | ex -> printfn "%A" (ex.ToString())

    0
